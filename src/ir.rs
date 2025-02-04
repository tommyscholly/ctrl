use std::{collections::HashMap, str::FromStr};

// the goal of the IR is to be a typed representation of the program, and also a simplified version of the program
use strum_macros::EnumString;

use crate::parse::{Bop, Expression};

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
    Hole,
    Unit,
    Any,
    TypeId(String), // This should correspond to a type in the type_map inside the type_checker
    Record(Vec<(String, T)>), // Where each field is sorted alphabetically
    BuiltIn(BuiltinType),
    Array(Box<T>, u32),
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
            return Ok(T::BuiltIn(t));
        } else {
            return Ok(T::TypeId(s.to_string()));
            // need to somehow construct records from just a token identifier
            // perhaps we just create a 'TypeId' type, that then is resolved and replaced
            // inside the typechecker
            // panic!("Ident is not a valid type") // TODO: this might not work now with
            // records once we add them
        };

        Err(TError::InvalidType)
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
struct Block(Vec<TypedIR>);

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
}

#[derive(Debug, Clone)]
struct TypedIR(IR, T);

pub type TypeInfo = HashMap<String, T>;

impl TypedIR {
    pub fn new(e: Expression, type_info: &mut TypeInfo) -> Self {
        use Expression::*;
        match e {
            Break => Self(IR::ControlFlow(Box::new(ControlFlow::Break)), T::Unit),
            Assignment { ident, binding } => {
                let binding = Box::new(Self::new(*binding, type_info));
                let ty = binding.type_of();
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
                        ControlFlow::Return((TypedIR(_, t))) => t,
                        _ => T::Hole,
                    }
                } else {
                    T::Hole
                };

                Self(IR::Block(insts), ret_type)
            }
            Infix {
                finished,
                operation,
                lhs,
                rhs,
            } => {
                let lhs = Box::new(Self::new(*lhs, type_info));
                let rhs = Box::new(Self::new(*rhs, type_info));
                assert_eq!(lhs.1, rhs.1);
                let ty = rhs.1.clone();

                let infix = IR::Infix(lhs, operation, rhs);
                Self(infix, ty)
            }

            _ => todo!(),
        }
    }

    pub fn type_of(&self) -> T {
        self.1.clone()
    }
}
