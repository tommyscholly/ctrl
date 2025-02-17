use crate::ir::{BuiltinType, T};
use cranelift::codegen::ir::types::{Type, F64, I32, I64, I8};

pub fn type_to_cranelift(ty: &T) -> Option<Type> {
    use T::{Any, Array, BuiltIn, Function, Hole, Record, TypeId, Unit};

    match ty {
        Hole => panic!("Hit bottom type when translating to IR"),
        Unit => None,
        BuiltIn(b) => match b {
            BuiltinType::Int => Some(I32),
            BuiltinType::Float => Some(F64),
            BuiltinType::String => Some(I64), // ptr
            BuiltinType::Char | BuiltinType::Bool => Some(I8),
        },
        Array(_, _) => Some(I64), // ptr
        Record(_)
        | Function {
            param_tys: _,
            return_ty: _,
        }
        | TypeId(_)
        | Any => Some(I64), // ptr
    }
}
