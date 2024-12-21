use std::collections::{HashMap, VecDeque};

use crate::lex::{take_block, take_through, take_until, validate_next_token, Token, TokenStream};

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Bop {
    Eql,
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

#[derive(Debug, Clone, PartialEq, Eq)]
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
    /// Type constructor for arrays, `Array a : Type -> Type`
    Array,
}

// An enum for all possible types
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum T {
    Hole,
    Unit,
    Record,
    BuiltIn(BuiltinType),
    Function {
        param_tys: Vec<T>,
        return_ty: Box<T>,
    },
}

type TypeMap = HashMap<String, T>;

trait Type {
    fn type_of(&self, type_map: &TypeMap) -> T;
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Literal {
    Bool(bool),
    Int(i64),
}

impl Type for Literal {
    fn type_of(&self, _: &TypeMap) -> T {
        use Literal::*;

        match *self {
            Bool(_) => T::BuiltIn(BuiltinType::Bool),
            Int(_) => T::BuiltIn(BuiltinType::Int),
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Block {
    instructions: Vec<Expression>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Expression {
    Literal(Literal),
    Identifier(String),
    Assignment {
        ident: String,
        binding: Box<Expression>,
    },
    Infix {
        operation: Bop,
        lhs: Box<Expression>,
        rhs: Box<Expression>,
    },
    Block(Block),
    IfElse {
        cond: Box<Expression>,
        then_block: Block,
        else_block: Option<Block>,
    },
}

impl Type for Expression {
    fn type_of(&self, type_map: &TypeMap) -> T {
        use Expression::*;

        match self {
            Block(_) => T::Unit,
            IfElse {
                cond: _,
                then_block: _,
                else_block: _,
            } => T::Unit,
            Infix {
                operation: _,
                lhs,
                rhs,
            } => {
                let lty = lhs.type_of(type_map);
                let rty = rhs.type_of(type_map);
                if lty == rty {
                    lty
                } else {
                    T::Hole
                }
            }
            Literal(l) => l.type_of(type_map),
            Assignment { ident: _, binding } => binding.type_of(type_map),
            Identifier(name) => {
                let ty = type_map.get(name);
                match ty {
                    Some(t) => t.clone(),
                    None => T::Hole,
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

        let assignment = match take_through(Token::SemiColon, &mut token_stream) {
            Some(a) => a,
            None => return Err(token_stream),
        };

        let mut assignment_ast = match parse(assignment) {
            Ok(ast) => ast,
            Err(_) => return Err(token_stream),
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

pub fn parse_infix(lhs: Expression, bop: Bop, mut token_stream: TokenStream<'_>) -> ParseResult {
    let rhs_tokens = match take_through(Token::SemiColon, &mut token_stream) {
        Some(a) => a,
        None => return Err(token_stream),
    };

    let mut rhs_ast = match parse(rhs_tokens) {
        Ok(ast) => ast,
        Err(_) => return Err(token_stream),
    };
    assert_eq!(rhs_ast.len(), 1);
    // safe to pop because one elem
    let rhs = rhs_ast.pop().unwrap();

    let ast = Expression::Infix {
        operation: bop,
        lhs: Box::new(lhs),
        rhs: Box::new(rhs),
    };

    Ok((ast, token_stream))
}

#[derive(Debug, PartialEq, Eq)]
pub enum ParseError {
    IncorrectLetBind,
    BlockMissing,
    General(String),
    MalformedInfix,
}

pub fn parse_block(mut toks: VecDeque<Token>) -> Result<Block, ParseError> {
    // remove the { and }
    toks.pop_front();
    toks.pop_back();

    let tokens = Vec::from(toks);
    let instructions = parse(tokens)?;

    let block = Block { instructions };

    Ok(block)
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

                let then_block_toks = match take_block(&mut token_stream) {
                    Some(toks) => toks,
                    None => return Err(ParseError::BlockMissing),
                };
                let then_block = parse_block(then_block_toks)?;

                let if_expr = if let Some(Token::Else) = token_stream.peek() {
                    token_stream.next(); // move past the else token
                    let else_block_toks = match take_block(&mut token_stream) {
                        Some(toks) => toks,
                        None => return Err(ParseError::BlockMissing),
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

                ast.push(if_expr)
            }
            Token::Int(i) => ast.push(Expression::Literal(Literal::Int(*i))),
            Token::Bool(b) => ast.push(Expression::Literal(Literal::Bool(*b))),
            Token::Eql
            | Token::Lt
            | Token::Le
            | Token::Gt
            | Token::Ge
            | Token::Plus
            | Token::Mul
            | Token::Min
            | Token::Div => {
                let lhs = match ast.pop() {
                    Some(lhs) => lhs,
                    None => return Err(ParseError::MalformedInfix),
                };

                let bop = Bop::from_token(tok);
                let bop_expr_result = parse_infix(lhs, bop, token_stream);
                match bop_expr_result {
                    Ok((bop_expr, stream)) => {
                        token_stream = stream;
                        ast.push(bop_expr);
                    }
                    Err(_) => return Err(ParseError::MalformedInfix),
                }
            }
            Token::SemiColon => {}
            _ => return Err(ParseError::General(format!("{:?}", tok))),
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
        assert_eq!(ast, vec![expected_assignment])
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
        assert_eq!(ast, vec![expected_assignment])
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
            operation: Bop::Eql,
            lhs: Box::new(Expression::Literal(Literal::Int(5))),
            rhs: Box::new(Expression::Literal(Literal::Int(5))),
        };
        let expected_assignment = Expression::Assignment {
            ident: String::from("name"),
            binding: Box::new(expected_infix),
        };

        let ast = parse(tokens).unwrap();
        assert_eq!(ast, vec![expected_assignment])
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
        assert_eq!(ast, vec![expected1, expected2])
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
        assert_eq!(ast, vec![expected1, expected2])
    }

    #[test]
    fn test_binop_lex_parse() {
        let input = "let add = 1 + 2;";

        let expected_infix = Expression::Infix {
            operation: Bop::Plus,
            lhs: Box::new(Expression::Literal(Literal::Int(1))),
            rhs: Box::new(Expression::Literal(Literal::Int(2))),
        };

        let expected_assignment = Expression::Assignment {
            ident: String::from("add"),
            binding: Box::new(expected_infix),
        };

        let ast = parse(tokenize(input)).unwrap();
        assert_eq!(ast, vec![expected_assignment])
    }

    #[test]
    fn test_ifexpr() {
        let input = "if true { 1 }";

        let expected = Expression::IfElse {
            cond: Box::new(Expression::Literal(Literal::Bool(true))),
            then_block: Block {
                instructions: vec![Expression::Literal(Literal::Int(1))],
            },
            else_block: None,
        };

        let ast = parse(tokenize(input)).unwrap();
        assert_eq!(ast, vec![expected])
    }

    #[test]
    fn test_ifelseexpr() {
        let input = "if true { 1 } else { 2 }";

        let expected = Expression::IfElse {
            cond: Box::new(Expression::Literal(Literal::Bool(true))),
            then_block: Block {
                instructions: vec![Expression::Literal(Literal::Int(1))],
            },
            else_block: Some(Block {
                instructions: vec![Expression::Literal(Literal::Int(2))],
            }),
        };

        let ast = parse(tokenize(input)).unwrap();
        assert_eq!(ast, vec![expected])
    }

    #[test]
    fn test_should_fail_no_lhs() {
        let tokens = vec![Token::Eql];
        let ast_result = parse(tokens);
        assert_eq!(ast_result, Err(ParseError::MalformedInfix))
    }
}
