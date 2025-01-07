use std::collections::{HashMap, VecDeque};
use std::fmt;
use std::str::FromStr;

use itertools::Itertools;
use strum_macros::EnumString;

use crate::lex::{take_block, take_through, take_until, validate_next_token, Token, TokenStream};

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Bop {
    Eql,
    Neq,
    Le,
    Lt,
    Gt,
    Ge,
    Plus,
    Min,
    Mul,
    Div,
}

impl Bop {
    fn from_token(token: &Token) -> Bop {
        match token {
            Token::Eql => Bop::Eql,
            Token::Neq => Bop::Neq,
            Token::Le => Bop::Le,
            Token::Lt => Bop::Lt,
            Token::Ge => Bop::Ge,
            Token::Gt => Bop::Gt,

            Token::Plus => Bop::Plus,
            Token::Min => Bop::Min,
            Token::Mul => Bop::Mul,
            Token::Div => Bop::Div,
            _ => panic!("attempt to convert non-bop token into bop"),
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, EnumString, Hash)]
#[strum(serialize_all = "camelCase")]
pub enum BuiltinType {
    /// Unicode string
    String,
    /// Character
    Char,
    /// Integer number
    Int,
    Bool,
    /// Floating point number
    Float,
}

// An enum for all possible types
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
#[allow(unused)]
pub enum T {
    Hole,
    Unit,
    TypeId(String), // This should correspond to a type in the type_map inside the type_checker
    Record(Vec<(String, T)>), // Where each field is sorted alphabetically
    BuiltIn(BuiltinType),
    Array(Box<T>),
    Function {
        param_tys: Vec<T>,
        return_ty: Box<T>,
    },
}

impl T {
    pub fn is_numeric(&self) -> bool {
        match self {
            Self::BuiltIn(BuiltinType::Int) => true, // TODO: add float
            _ => false,
        }
    }

    // flatten nested types, such as functions
    pub fn final_ty(&self) -> &T {
        match self {
            Self::Function { return_ty, .. } => return_ty,
            Self::Array(t) => t,
            _ => self,
        }
    }

    // returns the type and the offset
    pub fn field_info(&self, field_name: &str) -> (&T, u32) {
        let mut offset = 0;

        match self {
            Self::Record(fields) => {
                for (f_name, ty) in fields {
                    if f_name == field_name {
                        return (ty, offset);
                    }

                    offset += ty.size_of();
                }
            }
            _ => panic!("field_offset called on non-record {:?}", self),
        }

        panic!("field not found")
    }

    // stack sizes are u32s in cranelift ir
    pub fn size_of(&self) -> u32 {
        match self {
            Self::Function { .. } => todo!(),
            Self::Hole => panic!("size_of called on T::Hole"),
            Self::Unit => 0,
            Self::BuiltIn(b) => {
                match b {
                    BuiltinType::Float => 64,
                    BuiltinType::Int => 32,
                    BuiltinType::Bool => 8,
                    BuiltinType::Char => 8,
                    BuiltinType::String => 64, // ptr
                }
            }
            Self::Array(_) => 64, // ptr
            Self::Record(fields) => {
                let mut size = 0;
                for (_, ty) in fields {
                    size += ty.size_of();
                }

                size
            }
            Self::TypeId(_) => 64, // ptr
        }
    }

    fn from_token(tok: &Token) -> Option<T> {
        match tok {
            Token::Id(id_name) => {
                if let Ok(t) = BuiltinType::from_str(id_name) {
                    Some(T::BuiltIn(t))
                } else {
                    Some(T::TypeId(id_name.clone()))
                    // need to somehow construct records from just a token identifier
                    // perhaps we just create a 'TypeId' type, that then is resolved and replaced
                    // inside the typechecker
                    // panic!("Ident is not a valid type") // TODO: this might not work now with
                    // records once we add them
                }
            }
            _ => None,
        }
    }
}

type TypeMap = HashMap<String, T>;

pub trait Type {
    fn type_of(&self, type_map: &TypeMap) -> T;
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Literal {
    Bool(bool),
    Int(i32),
    String(String),
}

impl Type for Literal {
    fn type_of(&self, _: &TypeMap) -> T {
        use Literal::*;

        match *self {
            Bool(_) => T::BuiltIn(BuiltinType::Bool),
            Int(_) => T::BuiltIn(BuiltinType::Int),
            String(_) => T::BuiltIn(BuiltinType::String),
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Block {
    pub instructions: Vec<Expression>,
    pub has_return: bool,
    pub return_expr: Option<Box<Expression>>,
}

impl Block {
    pub fn new(instructions: Vec<Expression>) -> Self {
        let mut has_return = false;
        let mut return_expr = None;
        if let Some(Expression::Return(e)) = instructions.last() {
            has_return = true;
            return_expr = Some(e.clone());
        }

        Self {
            instructions,
            has_return,
            return_expr,
        }
    }
}

type TypePairings = Vec<(String, T)>;

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Function {
    pub name: String,
    pub params: TypePairings,
    pub return_ty: T,
    pub body: Block,
}

impl Type for Function {
    fn type_of(&self, tm: &TypeMap) -> T {
        let param_tys: Vec<T> = self
            .params
            .iter()
            .map(|(_, t)| {
                if let T::TypeId(id) = t {
                    tm.get(id)
                        .unwrap_or_else(|| panic!("TypeId not found"))
                        .clone()
                } else {
                    t.clone()
                }
            })
            .collect();
        T::Function {
            param_tys,
            return_ty: Box::new(self.return_ty.clone()),
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Record {
    pub name: String,
    pub fields: TypePairings,
}

impl Record {
    pub fn new(name: String, mut fields: TypePairings) -> Self {
        fields.sort_by(|(a, _), (b, _)| a.cmp(b));
        Self { name, fields }
    }
}

impl Type for Record {
    fn type_of(&self, _: &TypeMap) -> T {
        T::Record(self.fields.clone())
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Array {
    pub ty: T,
    pub size: u32,
    pub elements: Vec<Expression>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
#[allow(unused)]
pub enum Expression {
    Array(Array),
    Assignment {
        ident: String,
        binding: Box<Expression>,
    },
    Block(Block),
    Break,
    Call(String, Vec<Expression>),
    FieldAccess(String, String), // record_name.field_name
    ForIn(String, Box<Expression>, Block),
    Function(Function),
    Identifier(String),
    IfElse {
        cond: Box<Expression>,
        then_block: Block,
        else_block: Option<Block>,
    },
    Index {
        array: Box<Expression>,
        index: Box<Expression>,
    },
    Infix {
        finished: bool,
        operation: Bop,
        lhs: Box<Expression>,
        rhs: Box<Expression>,
    },
    Literal(Literal),
    Loop(Block),
    RecordDefinition(Record),
    RecordInitialization(String, Vec<(String, Box<Expression>)>),
    Return(Box<Expression>),
}

impl Type for Expression {
    fn type_of(&self, type_map: &TypeMap) -> T {
        use Expression::*;

        match self {
            Array(a) => T::Array(a.ty.clone().into()),
            Return(e) => e.type_of(type_map),
            Index { array: a, index: _ } => {
                if let T::Array(ty) = a.type_of(type_map) {
                    *ty
                } else {
                    panic!("attempted to index a non-array")
                }
            }
            Block(_)
            | ForIn(_, _, _)
            | Loop(_)
            | Break
            | IfElse {
                cond: _,
                then_block: _,
                else_block: _,
            } => T::Unit,
            Infix {
                finished,
                operation,
                lhs,
                rhs: _,
            } => {
                if *finished {
                    match operation {
                        Bop::Neq | Bop::Eql | Bop::Le | Bop::Lt | Bop::Gt | Bop::Ge => {
                            T::BuiltIn(BuiltinType::Bool)
                        }
                        Bop::Min | Bop::Mul => T::BuiltIn(BuiltinType::Int),
                        Bop::Plus => {
                            if let T::BuiltIn(BuiltinType::String) = lhs.type_of(type_map) {
                                T::BuiltIn(BuiltinType::String)
                            } else {
                                T::BuiltIn(BuiltinType::Int)
                            }
                        }
                        Bop::Div => panic!("need to implement floats"),
                    }
                } else {
                    T::Hole
                }
            }
            Literal(l) => l.type_of(type_map),
            Assignment { .. } => T::Hole,
            Identifier(name) => {
                let ty = type_map.get(name);
                match ty {
                    Some(t) => t.clone(),
                    None => T::Hole,
                }
            }
            Function(func) => func.return_ty.clone(),
            Call(func_name, _) => {
                let fn_ty = type_map.get(func_name);
                match fn_ty {
                    Some(t) => t.clone(),
                    None => T::BuiltIn(BuiltinType::Int),
                }
            }
            RecordDefinition(r) => r.type_of(type_map),
            RecordInitialization(record_name, _) => type_map.get(record_name).unwrap().clone(),
            FieldAccess(record_name, field_name) => {
                let record_ty = type_map.get(record_name).unwrap();
                match record_ty {
                    T::Record(fields) => {
                        let (_, field_ty) = fields
                            .iter()
                            .filter(|(f_name, _)| f_name == field_name)
                            .collect::<Vec<&(String, T)>>()[0];

                        field_ty.clone()
                    }
                    _ => panic!("attempted to access a field on a non-record"),
                }
            }
        }
    }
}

pub type ParseResult<'a> = Result<(Expression, TokenStream<'a>), TokenStream<'a>>;

pub fn parse_let(mut token_stream: TokenStream<'_>) -> ParseResult {
    if let &Token::Id(name) = token_stream
        .peek()
        .expect("let should be followed by an identifier")
    {
        token_stream.next();
        match validate_next_token(Token::Assign, token_stream) {
            Ok(stream) => token_stream = stream,
            Err(stream) => return Err(stream),
        }

        // let assignment = match take_through(Token::SemiColon, &mut token_stream) {
        //     Some(a) => a,
        //     None => return Err(token_stream),
        // };

        let Some(assignment) = take_through(Token::SemiColon, &mut token_stream) else {
            return Err(token_stream);
        };

        // let mut assignment_ast = match parse(assignment) {
        //     Ok(ast) => ast,
        //     Err(_) => return Err(token_stream),
        // };

        let Ok(mut assignment_ast) = parse(assignment) else {
            return Err(token_stream);
        };
        assert_eq!(assignment_ast.len(), 1);

        // safe to pop because one elem
        let ast = Expression::Assignment {
            ident: name.to_string(),
            binding: Box::new(assignment_ast.pop().unwrap()),
        };

        Ok((ast, token_stream))
    } else {
        panic!("let not followed by identifier");
    }
}

fn parse_infix(ast: &mut Vec<Expression>, expr: Expression) -> Expression {
    if let Some(Expression::Infix {
        finished: false,
        operation: _,
        lhs: _,
        rhs: _,
    }) = ast.last()
    {
        let Expression::Infix {
            finished: _,
            operation,
            lhs,
            rhs: _,
        } = ast.pop().unwrap()
        else {
            unreachable!()
        };

        Expression::Infix {
            finished: true,
            operation,
            lhs,
            rhs: Box::new(expr),
        }
    } else {
        expr
    }
}

#[derive(Debug, PartialEq, Eq, thiserror::Error)]
pub enum ParseError {
    IncorrectLetBind,
    BlockMissing,
    General(String),
    MalformedInfix,
    MalformedFn,
    MalformedType(String),
    MalformedUnless,
    SemicolonExpected,
    ExprExpected,
}

impl fmt::Display for ParseError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            ParseError::IncorrectLetBind => write!(f, "IncorrectLetBind"),
            ParseError::BlockMissing => write!(f, "BlockMissing"),
            ParseError::General(msg) => write!(f, "General({msg})"),
            ParseError::MalformedInfix => write!(f, "MalformedInfix"),
            ParseError::MalformedFn => write!(f, "MalformedFn"),
            ParseError::MalformedType(msg) => write!(f, "MalformedType({msg})"),
            ParseError::MalformedUnless => write!(f, "MalformedUnless"),
            ParseError::SemicolonExpected => write!(f, "SemicolonExpected"),
            ParseError::ExprExpected => write!(f, "ExprExpected"),
        }
    }
}

fn parse_block(mut toks: VecDeque<Token>) -> Result<Block, ParseError> {
    // remove the { and }
    toks.pop_front();
    toks.pop_back();

    let tokens = Vec::from(toks);
    let instructions = parse(tokens)?;

    let block = Block::new(instructions);

    Ok(block)
}

fn parse_params(mut toks: VecDeque<Token>) -> Result<TypePairings, ParseError> {
    if toks.len() < 2 {
        return Err(ParseError::MalformedFn);
    }

    let lparen = toks.pop_front().unwrap();
    let rparen = toks.pop_back().unwrap();
    if lparen == Token::LParen {
        assert_eq!(rparen, Token::RParen);
    } else if lparen == Token::LBrace {
        assert_eq!(rparen, Token::RBrace);
    } else {
        panic!("missing matching parens/braces");
    }

    let mut params = Vec::new();

    while !toks.is_empty() {
        let param_name = toks.pop_front().expect("param name must exist");
        let _colon = toks.pop_front().expect("type colon must exist");
        let param_ty = toks.pop_front().expect("param type must exist");
        let Some(ty) = T::from_token(&param_ty) else {
            return Err(ParseError::MalformedType(param_ty.to_string()));
        };

        let Token::Id(param) = param_name else {
            return Err(ParseError::MalformedFn);
        };

        params.push((param, ty));

        if !toks.is_empty() {
            if let Token::Comma = toks.pop_front().unwrap() {
                // comma should exist, all we need to do is pop it
            } else {
                return Err(ParseError::MalformedFn);
            }
        }
    }

    Ok(params)
}

// idents have a few different conditions
fn parse_ident(
    ast: &mut Vec<Expression>,
    ident: String,
    token_stream: &mut TokenStream<'_>,
) -> Result<Expression, ParseError> {
    if let Some(Token::LParen) = token_stream.peek() {
        let param_exprs = take_through(Token::RParen, token_stream);

        let mut params: Vec<Expression> = Vec::new();
        if let Some(toks) = param_exprs {
            let mut param_toks = VecDeque::from(toks);
            param_toks.pop_front();
            if param_toks.len() == 1 && *param_toks.front().unwrap() == Token::RParen {
            } else {
                let mut next_expr: Vec<Token> = Vec::new();
                while param_toks.front().is_some() {
                    let tok = param_toks.pop_front().unwrap();
                    if tok == Token::Comma || tok == Token::RParen {
                        let mut expr = parse(next_expr)?;
                        assert_eq!(expr.len(), 1);

                        params.push(expr.pop().unwrap());
                        next_expr = Vec::new();
                    } else {
                        next_expr.push(tok);
                    }
                }
            }
        }

        let func_call = Expression::Call(ident, params);
        let checked_infix = parse_infix(ast, func_call); // returns an infix if that is the previous expression, otherwise just returns the passed in expr
        Ok(checked_infix)
    // Record initialization parsing
    } else if let Some(Token::LBrace) = token_stream.peek() {
        let field_assignments = take_through(Token::RBrace, token_stream);
        if field_assignments.is_none() {
            panic!("todo, field assignments must be some");
        }

        let mut fields: Vec<(String, Box<Expression>)> = Vec::new();

        let mut field_toks = field_assignments.unwrap().into_iter().peekable();
        field_toks.next();

        if field_toks.len() == 1 && *field_toks.peek().unwrap() == Token::RParen {
        } else {
            let mut next_expr = vec![];
            while field_toks.peek().is_some() {
                let Some([Token::Id(field_name), Token::Assign]) = field_toks.next_array() else {
                    return Err(ParseError::General(
                        "Type initialization must be of the form 'field_name = expr'".to_string(),
                    ));
                };

                // TODO: remove these nasty nested while loops
                while field_toks.peek().is_some() {
                    let tok = field_toks.next().unwrap();
                    if tok == Token::Comma || tok == Token::RBrace {
                        let mut expr = parse(next_expr)?;
                        assert_eq!(expr.len(), 1);

                        let e = Box::new(expr.pop().unwrap());
                        fields.push((field_name.to_string(), e));
                        next_expr = Vec::new();
                        break;
                    } else {
                        next_expr.push(tok);
                    }
                }
            }
        }

        // fields should always be in alphabetical order, regardless of how the user
        // defined them. this will result in potentially unoptimized struct layouts,
        // but those can be optimized in the IR lowering. for higher level operations,
        // like type checking, assuming the fields are alphabetical is more useful
        fields.sort_by(|(a_name, _), (b_name, _)| a_name.cmp(b_name));
        let expr = Expression::RecordInitialization(ident.clone(), fields);
        Ok(expr)
    } else if let Some(Token::Dot) = token_stream.peek() {
        let Some([Token::Dot, Token::Id(field_name)]) = token_stream.next_array() else {
            return Err(ParseError::General("Field access malformed".to_string()));
        };

        Ok(Expression::FieldAccess(ident, field_name.to_string()))
    } else if let Some(Token::Assign) = token_stream.peek() {
        token_stream.next();
        let assignment = match take_through(Token::SemiColon, token_stream) {
            Some(a) => a,
            None => return Err(ParseError::SemicolonExpected),
        };

        let mut assignment_ast = match parse(assignment) {
            Ok(ast) => ast,
            Err(_) => return Err(ParseError::General("Assignment malformed".to_string())),
        };

        assert_eq!(assignment_ast.len(), 1);
        let expr = Expression::Assignment {
            ident: ident.clone(),
            binding: Box::new(assignment_ast.pop().unwrap()),
        };

        Ok(expr)
    } else if let Some(Token::LBracket) = token_stream.peek() {
        // index into an ident
        token_stream.next();
        let mut index_toks = match take_through(Token::RBracket, token_stream) {
            Some(toks) => toks,
            None => return Err(ParseError::General("Indexing malformed".to_string())),
        };
        index_toks.pop(); // remove the trailing bracket

        let mut index_expr = parse(index_toks)?;
        assert_eq!(index_expr.len(), 1);

        let expr = Expression::Index {
            array: Box::new(Expression::Identifier(ident.clone())),
            index: Box::new(index_expr.pop().unwrap()),
        };

        let checked_infix = parse_infix(ast, expr); // returns an infix if
                                                    // that is the previous expression, otherwise just returns the passed in expr
        Ok(checked_infix)
    } else {
        let expr = Expression::Identifier(ident.clone());
        let checked_infix = parse_infix(ast, expr); // returns an infix if
                                                    // that is the previous expression, otherwise just returns the passed in expr
        Ok(checked_infix)
    }
}

pub fn parse(tokens: Vec<Token>) -> Result<Vec<Expression>, ParseError> {
    let mut ast = Vec::new();
    let mut token_stream = tokens.iter().peekable();

    while let Some(tok) = token_stream.next() {
        match tok {
            Token::Let => {
                let let_expr_result = parse_let(token_stream);
                match let_expr_result {
                    Ok((let_expr, stream)) => {
                        token_stream = stream;
                        ast.push(let_expr);
                    }
                    Err(_) => return Err(ParseError::IncorrectLetBind),
                }
            }
            Token::If => {
                let cond_tokens = take_until(Token::LBrace, &mut token_stream).unwrap();
                let mut cond_expr = parse(cond_tokens)?;
                assert_eq!(cond_expr.len(), 1);
                let cond = Box::new(cond_expr.pop().unwrap());

                // let then_block_toks = match take_block(&mut token_stream) {
                //     Some(toks) => toks,
                //     None => return Err(ParseError::BlockMissing),
                // };
                let Some(then_block_toks) = take_block(&mut token_stream) else {
                    return Err(ParseError::BlockMissing);
                };
                let then_block = parse_block(then_block_toks)?;

                let if_expr = if let Some(Token::Else) = token_stream.peek() {
                    token_stream.next(); // move past the else token

                    // let else_block_toks = match take_block(&mut token_stream) {
                    //     Some(toks) => toks,
                    //     None => return Err(ParseError::BlockMissing),
                    // };
                    let Some(else_block_toks) = take_block(&mut token_stream) else {
                        return Err(ParseError::BlockMissing);
                    };

                    let else_block = parse_block(else_block_toks)?;

                    Expression::IfElse {
                        cond,
                        then_block,
                        else_block: Some(else_block),
                    }
                } else {
                    Expression::IfElse {
                        cond,
                        then_block,
                        else_block: None,
                    }
                };

                ast.push(if_expr);
            }
            Token::For => {
                let for_toks = take_through(Token::In, &mut token_stream).unwrap();
                assert_eq!(for_toks.len(), 2);
                let Token::Id(loop_var) = &for_toks[0] else {
                    return Err(ParseError::General(
                        "Loop variable not an ident".to_string(),
                    ));
                };

                let mut loop_expr_toks = take_through(Token::LBrace, &mut token_stream).unwrap();
                loop_expr_toks.pop(); // remove the trailing brace

                let mut loop_expr = parse(loop_expr_toks)?;
                assert_eq!(loop_expr.len(), 1);

                let loop_block_toks = take_block(&mut token_stream).unwrap();
                let loop_block = parse_block(loop_block_toks)?;

                ast.push(Expression::ForIn(
                    loop_var.to_string(),
                    loop_expr.pop().unwrap().into(),
                    loop_block,
                ));
            }
            Token::Loop => {
                let loop_block_toks = take_block(&mut token_stream).unwrap();
                let loop_block = parse_block(loop_block_toks)?;

                ast.push(Expression::Loop(loop_block));
            }
            Token::LBracket => {
                let array_toks = take_through(Token::RBracket, &mut token_stream).unwrap();
                let mut elements: Vec<Expression> = Vec::new();

                let mut element_toks = VecDeque::from(array_toks);
                if element_toks.len() == 1 && *element_toks.front().unwrap() == Token::RBracket {
                } else {
                    let mut next_expr: Vec<Token> = Vec::new();
                    while element_toks.front().is_some() {
                        let tok = element_toks.pop_front().unwrap();
                        if tok == Token::Comma || tok == Token::RBracket {
                            let mut expr = parse(next_expr)?;
                            assert_eq!(expr.len(), 1);

                            elements.push(expr.pop().unwrap());
                            next_expr = Vec::new();
                        } else {
                            next_expr.push(tok);
                        }
                    }
                }

                let ty = if elements.is_empty() {
                    T::Hole
                } else {
                    elements[0].type_of(&HashMap::new())
                };

                let array = Array {
                    ty,
                    size: elements.len() as u32,
                    elements,
                };
                ast.push(Expression::Array(array));
            }
            Token::Fn => {
                assert!(token_stream.peek().is_some());
                let name_tok = token_stream.next().unwrap();
                let Token::Id(fn_name) = name_tok else {
                    return Err(ParseError::General(
                        "Function name not an ident".to_string(),
                    ));
                };

                let param_exprs = take_through(Token::RParen, &mut token_stream);
                let params = if let Some(params) = param_exprs {
                    let param_toks = VecDeque::from(params);
                    parse_params(param_toks)?
                } else {
                    return Err(ParseError::MalformedFn);
                };

                let return_ty = if let Some(Token::LBrace) = token_stream.peek() {
                    T::Unit
                } else {
                    let expected_colon = token_stream.next().unwrap();
                    assert_eq!(*expected_colon, Token::Colon);
                    let return_ty_toks = take_until(Token::LBrace, &mut token_stream)
                        .expect("Function does not have a function body after the signature");

                    // not handling generics currently, all types will just be one token long
                    assert_eq!(return_ty_toks.len(), 1);
                    if let Some(t) = T::from_token(&return_ty_toks[0]) {
                        t
                    } else {
                        return Err(ParseError::MalformedType(return_ty_toks[0].to_string()));
                    }
                };

                let mut block = match take_block(&mut token_stream) {
                    Some(toks) => parse_block(toks)?,
                    None => return Err(ParseError::BlockMissing),
                };

                if fn_name == "main" && !block.has_return {
                    // hard code a return 0 in the main function if the user does not define it themselves
                    // i think this is a code smell, and might be wrong
                    block
                        .instructions
                        .push(Expression::Return(Box::new(Expression::Literal(
                            Literal::Int(0),
                        ))));
                }

                let function = Expression::Function(Function {
                    name: fn_name.to_string(),
                    params,
                    return_ty,
                    body: block,
                });

                ast.push(function);
            }
            Token::Type => {
                let Some([Token::Id(type_name), Token::Assign]) = token_stream.next_array() else {
                    return Err(ParseError::General(
                        "Tokens after 'type' does not match expected form of 'type ident ='"
                            .to_string(),
                    ));
                };
                if let Some(c) = type_name.chars().next() {
                    if !c.is_uppercase() {
                        return Err(ParseError::MalformedType(
                            "Type name must start with an uppercase letter".to_string(),
                        ));
                    }
                } else {
                    panic!("ID token is empty");
                }

                let field_toks = take_through(Token::RBrace, &mut token_stream);
                let fields = if let Some(fields) = field_toks {
                    let field_toks = VecDeque::from(fields);
                    parse_params(field_toks)?
                } else {
                    return Err(ParseError::MalformedType("TODO".to_string()));
                };

                let record = Record::new(type_name.to_string(), fields);
                ast.push(Expression::RecordDefinition(record));
            }
            Token::Return => {
                let return_toks = take_through(Token::SemiColon, &mut token_stream);
                if return_toks.is_none() {
                    return Err(ParseError::SemicolonExpected);
                }

                let mut parsed_exprs = parse(return_toks.unwrap())?;
                assert_eq!(parsed_exprs.len(), 1);
                let ret_expr = parsed_exprs.pop().unwrap();

                ast.push(Expression::Return(Box::new(ret_expr)));
            }
            Token::Break => {
                ast.push(Expression::Break);
                if let Some(Token::SemiColon) = token_stream.peek() {
                    token_stream.next();
                } else if let Some(Token::Unless) = token_stream.peek() {
                } else {
                    return Err(ParseError::SemicolonExpected);
                }
                //     token_stream.next();
                //
                //     let Some(unless_expr) = take_through(Token::SemiColon, &mut token_stream)
                //     else {
                //         return Err(ParseError::ExprExpected);
                //     };
                //
                //     let mut parsed_exprs = parse(unless_expr)?;
                //     assert_eq!(parsed_exprs.len(), 1);
                //
                //     let unless_expr = parsed_exprs.pop().unwrap();
                //     // the syntax is 'break unless <expr>', where if <expr> is true, then we
                //     // shouldn't break
                //     let cond_expr = Expression::Infix {
                //         finished: true,
                //         operation: Bop::Eql,
                //         lhs: Box::new(unless_expr),
                //         rhs: Box::new(Expression::Literal(Literal::Bool(false))),
                //     };
                //     // we rewrite the break into an if
                //     ast.push(Expression::IfElse {
                //         cond: Box::new(cond_expr),
                //         then_block: Block::new(vec![Expression::Break]),
                //         else_block: None,
                //     });
                // } else {
                //     return Err(ParseError::SemicolonExpected);
                // }
            }
            Token::Unless => {
                let Some(expression_to_run_unless) = ast.pop() else {
                    return Err(ParseError::MalformedUnless);
                };

                match expression_to_run_unless {
                    Expression::Break | Expression::Return(_) => {}
                    _ => return Err(ParseError::MalformedUnless),
                };

                let Some(unless_expr) = take_through(Token::SemiColon, &mut token_stream) else {
                    return Err(ParseError::ExprExpected);
                };

                let mut parsed_exprs = parse(unless_expr)?;
                assert_eq!(parsed_exprs.len(), 1);

                let unless_expr = parsed_exprs.pop().unwrap();
                let cond_expr = Expression::Infix {
                    finished: true,
                    operation: Bop::Eql,
                    lhs: Box::new(unless_expr),
                    rhs: Box::new(Expression::Literal(Literal::Bool(false))),
                };

                ast.push(Expression::IfElse {
                    cond: Box::new(cond_expr),
                    then_block: Block::new(vec![expression_to_run_unless]),
                    else_block: None,
                });
            }
            Token::Int(i) => {
                let expr = Expression::Literal(Literal::Int(*i));
                let checked_infix = parse_infix(&mut ast, expr); // returns an infix if that is the previous expression, otherwise just returns the passed in expr
                ast.push(checked_infix);
            }
            Token::Bool(b) => {
                let expr = Expression::Literal(Literal::Bool(*b));
                let checked_infix = parse_infix(&mut ast, expr); // returns an infix if that is the previous expression, otherwise just returns the passed in expr
                ast.push(checked_infix);
            }
            Token::Quote => {
                let mut string = take_through(Token::Quote, &mut token_stream).unwrap();
                string.pop(); // remove the trailing quote

                let string = string.into_iter().map(|t| t.to_string()).join(" ");
                let expr = Expression::Literal(Literal::String(string));
                let checked_infix = parse_infix(&mut ast, expr); // returns an infix if that is the previous expression, otherwise just returns the passed in expr
                ast.push(checked_infix);
            }
            Token::Id(ident) => {
                let expr = parse_ident(&mut ast, ident.clone(), &mut token_stream)?;
                ast.push(expr);
            }
            Token::Eql
            | Token::Neq
            | Token::Lt
            | Token::Le
            | Token::Gt
            | Token::Ge
            | Token::Plus
            | Token::Mul
            | Token::Min
            | Token::Div => {
                let Some(lhs) = ast.pop() else {
                    return Err(ParseError::MalformedInfix);
                };

                let bop = Bop::from_token(tok);
                let dummy_infix = Expression::Infix {
                    finished: false,
                    operation: bop,
                    lhs: Box::new(lhs),
                    rhs: Box::new(Expression::Literal(Literal::Int(-69))), // this rhs value should never be read
                };
                ast.push(dummy_infix);
                // let bop_expr_result = parse_infix(lhs, bop, token_stream);
                // match bop_expr_result {
                //     Ok((bop_expr, stream)) => {
                //         token_stream = stream;
                //         ast.push(bop_expr);
                //     }
                //     Err(_) => return Err(ParseError::MalformedInfix),
                // }
            }
            Token::SemiColon => {}
            _ => return Err(ParseError::General(format!("{tok:?}"))),
        }
    }

    Ok(ast)
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::lex::tokenize;

    #[test]
    fn test_parse_let_int_binding() {
        let tokens = vec![
            Token::Let,
            Token::Id(String::from("name")),
            Token::Assign,
            Token::Int(5),
            Token::SemiColon,
        ];

        let expected_assignment = Expression::Assignment {
            ident: String::from("name"),
            binding: Box::new(Expression::Literal(Literal::Int(5))),
        };

        let ast = parse(tokens).unwrap();
        assert_eq!(ast, vec![expected_assignment]);
    }

    #[test]
    fn test_parse_let_bool_binding() {
        let tokens = vec![
            Token::Let,
            Token::Id(String::from("name")),
            Token::Assign,
            Token::Bool(true),
            Token::SemiColon,
        ];

        let expected_assignment = Expression::Assignment {
            ident: String::from("name"),
            binding: Box::new(Expression::Literal(Literal::Bool(true))),
        };

        let ast = parse(tokens).unwrap();
        assert_eq!(ast, vec![expected_assignment]);
    }

    #[test]
    fn test_parse_let_eql_binding() {
        let tokens = vec![
            Token::Let,
            Token::Id(String::from("name")),
            Token::Assign,
            Token::Int(5),
            Token::Eql,
            Token::Int(5),
            Token::SemiColon,
        ];

        let expected_infix = Expression::Infix {
            finished: true,
            operation: Bop::Eql,
            lhs: Box::new(Expression::Literal(Literal::Int(5))),
            rhs: Box::new(Expression::Literal(Literal::Int(5))),
        };
        let expected_assignment = Expression::Assignment {
            ident: String::from("name"),
            binding: Box::new(expected_infix),
        };

        let ast = parse(tokens).unwrap();
        assert_eq!(ast, vec![expected_assignment]);
    }

    #[test]
    fn test_parse_two_let_bindings() {
        let tokens = vec![
            Token::Let,
            Token::Id(String::from("name")),
            Token::Assign,
            Token::Bool(true),
            Token::SemiColon,
            Token::Let,
            Token::Id(String::from("other_name")),
            Token::Assign,
            Token::Bool(false),
            Token::SemiColon,
        ];
        let expected1 = Expression::Assignment {
            ident: String::from("name"),
            binding: Box::new(Expression::Literal(Literal::Bool(true))),
        };
        let expected2 = Expression::Assignment {
            ident: String::from("other_name"),
            binding: Box::new(Expression::Literal(Literal::Bool(false))),
        };

        let ast = parse(tokens).unwrap();
        assert_eq!(ast, vec![expected1, expected2]);
    }

    #[test]
    fn test_lex_and_parse() {
        // this is also testing ignoring tabs
        let input = "let name = true;
                     let other_name = false;";

        let expected1 = Expression::Assignment {
            ident: String::from("name"),
            binding: Box::new(Expression::Literal(Literal::Bool(true))),
        };
        let expected2 = Expression::Assignment {
            ident: String::from("other_name"),
            binding: Box::new(Expression::Literal(Literal::Bool(false))),
        };
        let ast = parse(tokenize(input)).unwrap();
        assert_eq!(ast, vec![expected1, expected2]);
    }

    #[test]
    fn test_binop_lex_parse() {
        let input = "let add = 1 + 2;";

        let expected_infix = Expression::Infix {
            finished: true,
            operation: Bop::Plus,
            lhs: Box::new(Expression::Literal(Literal::Int(1))),
            rhs: Box::new(Expression::Literal(Literal::Int(2))),
        };

        let expected_assignment = Expression::Assignment {
            ident: String::from("add"),
            binding: Box::new(expected_infix),
        };

        let ast = parse(tokenize(input)).unwrap();
        assert_eq!(ast, vec![expected_assignment]);
    }

    #[test]
    fn test_binop_with_ident_parse() {
        let input = "let t = 1; let add = t + 2;";

        let t_assign = Expression::Assignment {
            ident: String::from("t"),
            binding: Box::new(Expression::Literal(Literal::Int(1))),
        };

        let expected_infix = Expression::Infix {
            finished: true,
            operation: Bop::Plus,
            lhs: Box::new(Expression::Identifier("t".to_string())),
            rhs: Box::new(Expression::Literal(Literal::Int(2))),
        };

        let add_assign = Expression::Assignment {
            ident: String::from("add"),
            binding: Box::new(expected_infix),
        };

        let ast = parse(tokenize(input)).unwrap();
        assert_eq!(ast, vec![t_assign, add_assign]);
    }

    #[test]
    fn test_ifexpr() {
        let input = "if true { 1 }";

        let expected = Expression::IfElse {
            cond: Box::new(Expression::Literal(Literal::Bool(true))),
            then_block: Block::new(vec![Expression::Literal(Literal::Int(1))]),
            else_block: None,
        };

        let ast = parse(tokenize(input)).unwrap();
        assert_eq!(ast, vec![expected]);
    }

    #[test]
    fn test_ifelseexpr() {
        let input = "if true { 1 } else { 2 }";

        let expected = Expression::IfElse {
            cond: Box::new(Expression::Literal(Literal::Bool(true))),
            then_block: Block::new(vec![Expression::Literal(Literal::Int(1))]),
            else_block: Some(Block::new(vec![Expression::Literal(Literal::Int(2))])),
        };

        let ast = parse(tokenize(input)).unwrap();
        assert_eq!(ast, vec![expected]);
    }

    #[test]
    fn test_should_fail_no_lhs() {
        let tokens = vec![Token::Eql];
        let ast_result = parse(tokens);
        assert_eq!(ast_result, Err(ParseError::MalformedInfix));
    }

    #[test]
    fn test_parse_params() {
        // (x: int)
        let input = VecDeque::from(vec![
            Token::LParen,
            Token::Id("x".to_string()),
            Token::Colon,
            Token::Id("int".to_string()),
            Token::RParen,
        ]);

        let output = parse_params(input).unwrap();
        assert_eq!(
            output,
            vec![("x".to_string(), T::BuiltIn(BuiltinType::Int))]
        );
    }

    #[test]
    fn test_parse_params2() {
        // (x: int)
        let input = VecDeque::from(vec![
            Token::LParen,
            Token::Id("x".to_string()),
            Token::Colon,
            Token::Id("int".to_string()),
            Token::Comma,
            Token::Id("y".to_string()),
            Token::Colon,
            Token::Id("bool".to_string()),
            Token::RParen,
        ]);

        let output = parse_params(input).unwrap();
        assert_eq!(
            output,
            vec![
                ("x".to_string(), T::BuiltIn(BuiltinType::Int)),
                ("y".to_string(), T::BuiltIn(BuiltinType::Bool))
            ]
        );
    }

    #[test]
    fn test_parse_basic_fn() {
        let input = "fn basic() {}";
        let tokens = tokenize(input);
        let ast = parse(tokens).unwrap();

        let expected = Expression::Function(Function {
            name: String::from("basic"),
            params: vec![],
            return_ty: T::Unit,
            body: Block::new(vec![]),
        });
        assert_eq!(ast, vec![expected]);
    }

    #[test]
    fn test_parse_basic_fn_with_params_and_return() {
        let input = "fn basic(x: int, y: string, z: bool): int {}";
        let tokens = tokenize(input);
        let ast = parse(tokens).unwrap();

        let params = vec![
            ("x".to_string(), T::BuiltIn(BuiltinType::Int)),
            ("y".to_string(), T::BuiltIn(BuiltinType::String)),
            ("z".to_string(), T::BuiltIn(BuiltinType::Bool)),
        ];

        let expected = Expression::Function(Function {
            name: String::from("basic"),
            params,
            return_ty: T::BuiltIn(BuiltinType::Int),
            body: Block::new(vec![]),
        });
        assert_eq!(ast, vec![expected]);
    }

    #[test]
    fn test_full_fn() {
        let input = "
        fn basic(x: int, y: bool): int {
            if y {
                return x + 1;
            } else {
                return 0;
            }
        }";

        let tokens = tokenize(input);
        let ast = parse(tokens).unwrap();

        let params = vec![
            ("x".to_string(), T::BuiltIn(BuiltinType::Int)),
            ("y".to_string(), T::BuiltIn(BuiltinType::Bool)),
        ];

        let expected_infix = Expression::Infix {
            finished: true,
            operation: Bop::Plus,
            lhs: Box::new(Expression::Identifier("x".to_string())),
            rhs: Box::new(Expression::Literal(Literal::Int(1))),
        };

        let if_expr = Expression::IfElse {
            cond: Box::new(Expression::Identifier("y".to_string())),
            then_block: Block::new(vec![Expression::Return(Box::new(expected_infix))]),
            else_block: Some(Block::new(vec![Expression::Return(Box::new(
                Expression::Literal(Literal::Int(0)),
            ))])),
        };

        let expected = Expression::Function(Function {
            name: String::from("basic"),
            params,
            return_ty: T::BuiltIn(BuiltinType::Int),
            body: Block::new(vec![if_expr]),
        });

        assert_eq!(ast, vec![expected]);
    }

    #[test]
    fn test_fn_call() {
        let input = "test(1, thing)";
        let tokens = tokenize(input);
        let ast = parse(tokens).unwrap();

        let expected = Expression::Call(
            "test".to_string(),
            vec![
                Expression::Literal(Literal::Int(1)),
                Expression::Identifier("thing".to_string()),
            ],
        );
        assert_eq!(ast, vec![expected]);
    }

    #[test]
    fn test_actual_function_call() {
        let input = include_str!("../examples/func_call.ctrl");
        let tokens = tokenize(input);
        let ast = parse(tokens).unwrap();

        let test_fn = Expression::Function(Function {
            name: String::from("test"),
            params: vec![],
            return_ty: T::BuiltIn(BuiltinType::Int),
            body: Block::new(vec![Expression::Return(Box::new(Expression::Literal(
                Literal::Int(50),
            )))]),
        });

        let main_fn_body = vec![
            Expression::Assignment {
                ident: "t".to_string(),
                binding: Box::new(Expression::Call("test".to_string(), vec![])),
            },
            Expression::Return(Box::new(Expression::Identifier("t".to_string()))),
        ];

        let main_fn = Expression::Function(Function {
            name: String::from("main"),
            params: vec![],
            return_ty: T::BuiltIn(BuiltinType::Int),
            body: Block::new(main_fn_body),
        });

        assert_eq!(ast, vec![test_fn, main_fn]);
    }

    #[test]
    fn test_record_type() {
        let input = "type T = {x: int , y: bool}";
        let tokens = tokenize(input);
        let ast = parse(tokens).unwrap();

        let fields = vec![
            (String::from("x"), T::BuiltIn(BuiltinType::Int)),
            (String::from("y"), T::BuiltIn(BuiltinType::Bool)),
        ];
        let record = Record::new(String::from("T"), fields);
        let expected = Expression::RecordDefinition(record);
        assert_eq!(ast, vec![expected]);
    }

    #[test]
    fn test_record_lowercase() {
        let input = "type t = {}";
        let tokens = tokenize(input);
        let ast = parse(tokens);

        assert!(ast.is_err());
        assert_eq!(
            ast.unwrap_err(),
            ParseError::MalformedType("Type name must start with an uppercase letter".to_string(),)
        );
    }

    #[test]
    fn test_basic_record_initialization() {
        let input = "T {x = 1, y = true}";
        let tokens = tokenize(input);
        let ast = parse(tokens).unwrap();

        let fields = vec![
            (
                String::from("x"),
                Box::new(Expression::Literal(Literal::Int(1))),
            ),
            (
                String::from("y"),
                Box::new(Expression::Literal(Literal::Bool(true))),
            ),
        ];
        let expected = Expression::RecordInitialization(String::from("T"), fields);
        assert_eq!(ast, vec![expected]);
    }

    #[test]
    fn test_record_assignment() {
        let input = "let t = T {x = 1, y = true};";
        let tokens = tokenize(input);
        let ast = parse(tokens).unwrap();

        let fields = vec![
            (
                String::from("x"),
                Box::new(Expression::Literal(Literal::Int(1))),
            ),
            (
                String::from("y"),
                Box::new(Expression::Literal(Literal::Bool(true))),
            ),
        ];
        let record = Expression::RecordInitialization(String::from("T"), fields);
        let expected = Expression::Assignment {
            ident: String::from("t"),
            binding: Box::new(record),
        };
        assert_eq!(ast, vec![expected]);
    }

    #[test]
    fn test_record_field_access() {
        let input = "let t = T {x = 1, y = true}; t.x";
        let tokens = tokenize(input);
        let ast = parse(tokens).unwrap();

        let fields = vec![
            (
                String::from("x"),
                Box::new(Expression::Literal(Literal::Int(1))),
            ),
            (
                String::from("y"),
                Box::new(Expression::Literal(Literal::Bool(true))),
            ),
        ];
        let record = Expression::RecordInitialization(String::from("T"), fields);
        let expected_assign = Expression::Assignment {
            ident: String::from("t"),
            binding: Box::new(record),
        };
        let expected_access = Expression::FieldAccess(String::from("t"), String::from("x"));
        assert_eq!(ast, vec![expected_assign, expected_access]);
    }

    #[test]
    fn test_string_parse() {
        let input = r#""hello world""#;
        let tokens = tokenize(input);
        let ast = parse(tokens).unwrap();

        let expected = Expression::Literal(Literal::String("hello world".to_string()));
        assert_eq!(ast, vec![expected]);
    }

    #[test]
    fn test_string_parse_with_num_in_string() {
        let input = r#""hello 123""#;
        let tokens = tokenize(input);
        let ast = parse(tokens).unwrap();

        let expected = Expression::Literal(Literal::String("hello 123".to_string()));
        assert_eq!(ast, vec![expected]);
    }

    #[test]
    fn test_loop() {
        let input = "loop { print_int(1); break; }";
        let tokens = tokenize(input);
        let ast = parse(tokens).unwrap();

        let expected = Expression::Loop(Block::new(vec![
            Expression::Call(
                "print_int".to_string(),
                vec![Expression::Literal(Literal::Int(1))],
            ),
            Expression::Break,
        ]));
        assert_eq!(ast, vec![expected]);
    }

    #[test]
    fn test_parse_arr() {
        let input = "[1, 2, 3]";
        let tokens = tokenize(input);
        let ast = parse(tokens).unwrap();

        let expected = Expression::Array(Array {
            ty: T::BuiltIn(BuiltinType::Int),
            size: 3,
            elements: vec![
                Expression::Literal(Literal::Int(1)),
                Expression::Literal(Literal::Int(2)),
                Expression::Literal(Literal::Int(3)),
            ],
        });
        assert_eq!(ast, vec![expected]);
    }

    #[test]
    fn test_index_arr() {
        let input = "let arr = [1, 2, 3]; arr[0]";

        let tokens = tokenize(input);
        let ast = parse(tokens).unwrap();

        let arr = Expression::Array(Array {
            ty: T::BuiltIn(BuiltinType::Int),
            size: 3,
            elements: vec![
                Expression::Literal(Literal::Int(1)),
                Expression::Literal(Literal::Int(2)),
                Expression::Literal(Literal::Int(3)),
            ],
        });

        let expected_assign = Expression::Assignment {
            ident: "arr".to_string(),
            binding: Box::new(arr),
        };

        let expected_index = Expression::Index {
            array: Box::new(Expression::Identifier("arr".to_string())),
            index: Box::new(Expression::Literal(Literal::Int(0))),
        };

        assert_eq!(ast, vec![expected_assign, expected_index]);
    }

    #[test]
    fn test_break_unless() {
        let input = "break unless true;";
        let tokens = tokenize(input);
        let ast = parse(tokens).unwrap();

        let expected = Expression::IfElse {
            cond: Box::new(Expression::Infix {
                finished: true,
                operation: Bop::Eql,
                lhs: Expression::Literal(Literal::Bool(true)).into(),
                rhs: Expression::Literal(Literal::Bool(false)).into(),
            }),
            then_block: Block::new(vec![Expression::Break]),
            else_block: None,
        };
        assert_eq!(ast, vec![expected]);
    }

    #[test]
    fn test_malformed_break() {
        let input = "break let t = 2;";

        let tokens = tokenize(input);
        let ast = parse(tokens);

        assert!(ast.is_err());
        assert_eq!(ast.unwrap_err(), ParseError::SemicolonExpected);
    }

    // TODO: have this error in the parser rather than the type checker
    #[ignore = "this is handled in the type checker, needs to be moved here"]
    #[test]
    fn test_malformed_break_unless() {
        let input = "break unless let t = 2;";
        let tokens = tokenize(input);
        let ast = parse(tokens);
        assert!(ast.is_err());
    }
}
