use std::{collections::HashMap, str::FromStr};

use itertools::Itertools;
// the goal of the IR is to be a typed representation of the program, and also a simplified version of the program
use strum_macros::EnumString;

use crate::parse::{self, Bop, Expression};

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Literal {
    Bool(bool),
    Int(i32),
    String(String),
}

#[derive(Debug, Clone, PartialEq, Eq, EnumString, Hash)]
#[strum(serialize_all = "camelCase")]
pub enum BuiltinType {
    // Unicode string
    String,
    // Unicode char
    Char,
    Int,
    Bool,
    Float,
}

// An enum for all possible types
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
#[allow(unused)]
pub enum T {
    Hole, // Holes mark expressions that cannot be type checked, or that need to be filled in
    // during type checking. the former is a loop used as a value, the latter is a function
    // call
    Unit,
    Any,
    TypeId(String), // This should correspond to a type in the type_map inside the type_checker
    Record(Vec<(String, T)>), // Where each field is sorted alphabetically
    BuiltIn(BuiltinType),
    Array(Box<T>, usize),
    Function {
        param_tys: Vec<T>,
        return_ty: Box<T>,
    },
}

#[derive(Debug, Clone)]
pub enum TError {
    InvalidType,
}

impl FromStr for T {
    type Err = TError;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        if let Ok(t) = BuiltinType::from_str(s) {
            Ok(T::BuiltIn(t))
        } else {
            Ok(T::TypeId(s.to_string()))
            // need to somehow construct records from just a token identifier
            // perhaps we just create a 'TypeId' type, that then is resolved and replaced
            // inside the typechecker
            // panic!("Ident is not a valid type") // TODO: this might not work now with
            // records once we add them
        }

        // Err(TError::InvalidType)
    }
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
            Self::Array(t, _) => t,
            _ => self,
        }
    }

    pub fn from_lit(lit: Literal) -> T {
        use Literal::*;
        match lit {
            Bool(_) => Self::BuiltIn(BuiltinType::Bool),
            Int(_) => Self::BuiltIn(BuiltinType::Int),
            String(_) => Self::BuiltIn(BuiltinType::String),
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
            Self::Hole => panic!("size_of called on T::Hole"),
            Self::Unit => 0,
            Self::Function { .. } => todo!(),
            Self::BuiltIn(b) => {
                match b {
                    BuiltinType::Float => 64,
                    BuiltinType::Int => 32,
                    BuiltinType::Bool => 8,
                    BuiltinType::Char => 8,
                    BuiltinType::String => 64, // ptr
                }
            }
            Self::Array(_, _) => 64, // ptr
            Self::Record(fields) => {
                let mut size = 0;
                for (_, ty) in fields {
                    size += ty.size_of();
                }

                size
            }
            Self::TypeId(_) => 64, // ptr
            Self::Any => 64,       // ptr
        }
    }
}

// the has return fields are no longer needed, we can just get the last element of the vector then
// check if it's of type ControlFlow
#[derive(Debug, Clone)]
struct Body(Vec<TypedIR>);

impl Body {
    fn from_body(block: parse::Block, type_info: &mut TypeInfo) -> Self {
        let insts: Vec<TypedIR> = block
            .instructions
            .into_iter()
            .map(|inst| TypedIR::new(inst, type_info))
            .collect();

        Self(insts)
    }
}

#[derive(Debug, Clone)]
enum ControlFlow {
    Break,
    Return(TypedIR),
    Continue,
}

#[derive(Debug, Clone)]
enum IR {
    ControlFlow(Box<ControlFlow>),
    Assignment(String, Box<TypedIR>),
    TypeDefinition(String),
    // the has return fields are no longer needed, we can just get the last element of the vector then
    // check if it's of type ControlFlow
    Block(Vec<TypedIR>),
    Infix(Box<TypedIR>, Bop, Box<TypedIR>),
    Literal(Literal),
    Array(Vec<TypedIR>),
    Index(Box<TypedIR>, Box<TypedIR>),
    Identifier(String),
    IfElse(Box<TypedIR>, Body, Body),
    Loop(Body),
    // func name, param tys, block
    // note: not sure if we can drop param names yet, i don't believe so
    Function(String, Vec<(String, T)>, Body),
    Call(String, Vec<TypedIR>),
}

#[derive(Debug, Clone)]
pub struct TypedIR(IR, T);

pub type TypeInfo = HashMap<String, T>;

impl TypedIR {
    pub fn new(e: Expression, type_info: &mut TypeInfo) -> Self {
        use Expression::*;
        match e {
            IfElse {
                cond,
                then_block,
                else_block,
            } => {
                let cond = Box::new(Self::new(*cond, type_info));
                let then_block = Body::from_body(then_block, type_info);
                let else_block =
                    else_block.map_or(Body(vec![]), |block| Body::from_body(block, type_info));
                Self(IR::IfElse(cond, then_block, else_block), T::Unit)
            }
            Identifier(ident) => {
                // this might not be smart if an identifier is used before it's defined
                let ty = match type_info.get(&ident) {
                    Some(ty) => ty.clone(),
                    None => T::Hole,
                };
                Self(IR::Identifier(ident), ty)
            }
            Index { array, index } => {
                let array_or_identifier = Box::new(Self::new(*array, type_info));
                let index = Box::new(Self::new(*index, type_info));
                Self(IR::Index(array_or_identifier, index), T::Hole)
            }
            Literal(lit) => Self(IR::Literal(lit.clone()), T::from_lit(lit)),
            Loop(body) => {
                let body = Body::from_body(body, type_info);
                Self(IR::Loop(body), T::Hole)
            }
            Call(func_name, params) => {
                let params: Vec<Self> = params
                    .into_iter()
                    .map(|expr| Self::new(expr, type_info))
                    .collect();
                // in our type checking/type rewriting phase we need to fill this hole in
                Self(IR::Call(func_name, params), T::Hole)
            }
            Function(func) => {
                let name = func.name;
                let params: Vec<(String, T)> = func
                    .params
                    .into_iter()
                    .map(|(name, ty_str)| {
                        let ty = T::from_str(&ty_str).unwrap();
                        (name, ty)
                    })
                    .collect();

                let param_tys = params.iter().cloned().map(|(_, t)| t).collect();

                let return_ty = func
                    .return_ty
                    .map_or(T::Unit, |ty_str| T::from_str(&ty_str).unwrap());

                let func_ty = T::Function {
                    param_tys,
                    return_ty: Box::new(return_ty),
                };

                type_info.insert(name.clone(), func_ty.clone());
                let func_block = Body::from_body(func.body, type_info);
                let function = IR::Function(name, params, func_block);

                Self(function, func_ty)
            }
            Array(a) => {
                let elems = a.elements;
                if elems.is_empty() {
                    let arr = IR::Array(vec![]);
                    return Self(arr, T::Hole);
                }

                let elems: Vec<TypedIR> =
                    elems.into_iter().map(|e| Self::new(e, type_info)).collect();
                let elems_len = elems.len();

                let mut elem_tys: Vec<T> = elems.iter().cloned().map(|ty_expr| ty_expr.1).collect();

                if !elem_tys.iter().all_equal() {
                    panic!("array elements expected to have type {:?}", elem_tys[0]);
                }

                let ty = elem_tys.pop().unwrap();
                Self(IR::Array(elems), T::Array(Box::new(ty), elems_len))
            }
            Return(expr) => {
                let ty_expr = Self::new(*expr, type_info);
                let ty = ty_expr.1.clone();
                let return_expr = ControlFlow::Return(ty_expr);
                Self(IR::ControlFlow(Box::new(return_expr)), ty)
            }
            Break => Self(IR::ControlFlow(Box::new(ControlFlow::Break)), T::Unit),
            Assignment { ident, binding } => {
                let binding = Box::new(Self::new(*binding, type_info));
                let ty = binding.type_of();
                type_info.insert(ident.clone(), ty.clone());
                Self(IR::Assignment(ident, binding), ty)
            }
            RecordDefinition(r) => {
                let fields = r
                    .fields
                    .into_iter()
                    .map(|(name, ty_str)| {
                        let ty = T::from_str(&ty_str).unwrap();
                        (name, ty)
                    })
                    .collect();

                let ty = T::Record(fields);
                type_info.insert(r.name.clone(), ty.clone());
                Self(IR::TypeDefinition(r.name), ty)
            }
            Block(b) => {
                let insts: Vec<TypedIR> = b
                    .instructions
                    .into_iter()
                    .map(|e| Self::new(e, type_info))
                    .collect();

                let ret_type = if let Some(TypedIR(IR::ControlFlow(c), _)) = insts.last() {
                    let c_clone = c.clone();
                    match *c_clone {
                        ControlFlow::Return(TypedIR(_, t)) => t,
                        _ => T::Hole,
                    }
                } else {
                    T::Hole
                };

                Self(IR::Block(insts), ret_type)
            }
            Infix {
                finished: _finished,
                operation,
                lhs,
                rhs,
            } => {
                let lhs = Box::new(Self::new(*lhs, type_info));
                let rhs = Box::new(Self::new(*rhs, type_info));

                let ty = match operation {
                    Bop::Plus => T::BuiltIn(BuiltinType::Int),
                    Bop::Min => T::BuiltIn(BuiltinType::Int),
                    Bop::Mul => T::BuiltIn(BuiltinType::Int),
                    Bop::Div => T::BuiltIn(BuiltinType::Int),
                    Bop::Eql => T::BuiltIn(BuiltinType::Bool),
                    Bop::Le => T::BuiltIn(BuiltinType::Bool),
                    Bop::Lt => T::BuiltIn(BuiltinType::Bool),
                    Bop::Ge => T::BuiltIn(BuiltinType::Bool),
                    Bop::Gt => T::BuiltIn(BuiltinType::Bool),
                    Bop::Neq => T::BuiltIn(BuiltinType::Bool),
                };

                let infix = IR::Infix(lhs, operation, rhs);
                Self(infix, ty)
            }
            e => {
                println!("{:?}", e);

                todo!()
            }
        }
    }

    pub fn type_of(&self) -> T {
        self.1.clone()
    }
}

pub fn default_type_info() -> TypeInfo {
    let mut type_info = TypeInfo::new();
    let print_int = T::Function {
        param_tys: vec![T::BuiltIn(BuiltinType::Int)],
        return_ty: Box::new(T::Unit),
    };
    type_info.insert("print_int".to_string(), print_int);

    type_info
}

fn derive_individual_type(ir: &mut TypedIR, type_info: &mut TypeInfo) -> bool {
    if ir.1 == T::Hole {
        match &ir.0 {
            IR::Identifier(i) => {
                if let Some(ty) = type_info.get(i) {
                    ir.1 = ty.clone();
                } else {
                    eprintln!("no type found for {}", i);
                    return false;
                }
            }
            e => {
                eprintln!("got {:?}", e);
                return false;
            }
        }
    }

    true
}

/*
 * Iterate through the IR, recursing downwards in any sub expressions
 * Ensure that identifiers have their types replaced
 */
pub fn type_check(
    ir: &mut [TypedIR],
    type_info: &mut TypeInfo,
    expected_return: Option<T>,
) -> bool {
    for idx in 0..ir.len() {
        let expr = ir.get_mut(idx).unwrap();

        if !type_check_expr(expr, type_info, expected_return.clone()) {
            return false;
        }
    }

    true
}

fn type_check_expr(
    expr: &mut TypedIR,
    type_info: &mut TypeInfo,
    expected_return: Option<T>,
) -> bool {
    let ty = expr.type_of();
    let expr_ir = &mut expr.0;

    match expr_ir {
        IR::IfElse(cond, if_block, else_block) => {
            let cond = cond.as_mut();
            if !derive_individual_type(cond, type_info)
                || cond.type_of() != T::BuiltIn(BuiltinType::Bool)
            {
                eprintln!(
                    "if condition did not evaluate to bool, got {:?}",
                    cond.type_of()
                );
                return false;
            }

            if !type_check(&mut if_block.0, type_info, expected_return.clone()) {
                eprintln!("if block");
                return false;
            }

            if !type_check(&mut else_block.0, type_info, expected_return.clone()) {
                eprintln!("else block");
                return false;
            }
        }
        IR::Loop(b) => {
            return type_check(&mut b.0, type_info, expected_return);
        }
        IR::Call(func_name, params) => {
            let function = type_info.get_mut(func_name);
            if !type_check(params, type_info, expected_return.clone()) {
                panic!("here");
            }

            let func_ty = type_info.get_mut(func_name);
            if let Some(T::Function {
                param_tys,
                return_ty,
            }) = func_ty
            {
                let param_tys = param_tys.clone();
                let return_ty = return_ty.clone();

                for (param, ty) in params.iter_mut().zip(param_tys) {
                    let param_ty = param.type_of();
                    if param_ty != ty {
                        eprintln!("parameter {param:?}: expected {ty:?}, got {param_ty:?}");
                        return false;
                    }
                }

                expr.1 = *return_ty;
            } else {
                eprintln!("function {func_name} undefined");
                return false;
            }
        }
        IR::Index(arr, idx) => {
            if T::BuiltIn(BuiltinType::Int) != idx.1 {
                eprintln!("idx expected to be type int, got {:?}", idx.1);
                return false;
            }

            match arr.type_of() {
                T::Array(t, _) => expr.1 = *t.clone(),
                _ => {
                    eprintln!("expected array in index operation, got {:?}", arr);
                    return false;
                }
            };
        }
        IR::Block(b) => {
            return type_check(b, type_info, expected_return);
        }
        IR::TypeDefinition(_) => {}
        IR::Function(_name, params, body) => {
            if let T::Function {
                param_tys: _,
                return_ty,
            } = ty
            {
                let return_ty = if T::Hole == *return_ty {
                    None
                } else {
                    Some(*return_ty)
                };

                let mut new_type_info = type_info.clone();
                for (param, ty) in params {
                    new_type_info.insert(param.clone(), ty.clone());
                }

                if !type_check(&mut body.0, &mut new_type_info, return_ty) {
                    eprintln!("function did not type check");
                    return false;
                }
            } else {
                eprintln!("function doesn't have function type?");
                return false;
            }
        }
        IR::Identifier(id) => {
            if let Some(t) = type_info.get(id) {
                expr.1 = t.clone();
            }
        }
        IR::Infix(lhs, operation, rhs) => {
            if !type_check_expr(lhs, type_info, expected_return.clone())
                || !type_check_expr(rhs, type_info, expected_return.clone())
            {
                eprintln!("infix did not type check");
                return false;
            }

            let lhs_ty = lhs.type_of();
            let rhs_ty = rhs.type_of();

            if lhs_ty != rhs_ty {
                println!("infix did not type check {lhs_ty:?} != {rhs_ty:?}");
                return false;
            }

            expr.1 = ty;
        }
        IR::Assignment(name, assign_expr) => {
            if !type_check_expr(assign_expr, type_info, expected_return) {
                eprintln!("assignment did not type check");
                return false;
            }
            let expr_ty = assign_expr.type_of();
            let assign_ty = type_info.get(name); // a side-effect here is we "check" assignments that are first assigns, as well as reassigns
            if let Some(assign_ty) = assign_ty {
                if *assign_ty == T::Hole {
                    expr.1 = expr_ty.clone();
                    type_info.insert(name.clone(), expr_ty.clone());
                } else if expr_ty != *assign_ty {
                    eprintln!(
                        "ident {} has type {:?}, assigned to type {:?}",
                        name, assign_ty, expr_ty
                    );
                    return false;
                }
            } else {
                panic!("idk does this even happen?");
            }
        }
        IR::Array(_) => {} // we type arrays when we convert from the other IR
        IR::Literal(_) => {}
        IR::ControlFlow(flow) => {
            if let ControlFlow::Return(mut expr) = *flow.clone() {
                if !type_check_expr(&mut expr, type_info, expected_return.clone()) {
                    eprintln!("failed to typecheck return expr {expr:?}");
                    return false;
                }

                if let Some(ref ty) = expected_return {
                    assert_eq!(&expr.1, ty)
                } else {
                    eprintln!("not in a returnable state");
                    return false;
                }
            }
        }
    }

    true
}
