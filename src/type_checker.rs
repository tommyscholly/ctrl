use std::{collections::HashMap, iter::zip};

use strum_macros::Display;

use crate::parse::{Bop, BuiltinType, Expression, Type, T};

#[derive(Debug, Display, PartialEq, Eq)]
pub enum TypeError {
    NotDefined(String),
    CallNonFunction(String),
    ParameterTypeMismatch(String, T, T),
    #[strum(to_string = "ReturnTypeMismatch (func: {0}, got: {1:?}, expected: {2:?})")]
    ReturnTypeMismatch(String, T, T),
    InfixTypeMismatch(T, T),
    NumericInfix(T),
    ConditionNotBoolean(T),
}

pub struct TypeChecker {
    type_map: HashMap<String, T>,
}

impl TypeChecker {
    pub fn new() -> Self {
        Self {
            type_map: HashMap::new(),
        }
    }

    fn check_expr(&mut self, expr: &Expression) -> Result<(), TypeError> {
        use Expression::*;
        match expr {
            Function(f) => {
                self.type_check(&f.body.instructions)?;
                if f.body.has_return {
                    let f_ty = self.type_map.get(&f.name).unwrap();
                    let T::Function { return_ty, .. } = f_ty else {
                        return Err(TypeError::CallNonFunction(f.name.to_string()));
                    };

                    let return_expr = f.body.return_expr.as_ref().unwrap();
                    let ret_ty = return_expr.type_of(&self.type_map);
                    if *ret_ty.final_ty() != **return_ty {
                        return Err(TypeError::ReturnTypeMismatch(
                            f.name.to_string(),
                            ret_ty,
                            *return_ty.clone(),
                        ));
                    }
                }
            }
            Call(f_name, params) => {
                let f_ty = self.type_map.get(f_name);
                if let Some(t) = f_ty {
                    let T::Function { param_tys, .. } = t else {
                        return Err(TypeError::CallNonFunction(f_name.to_string()));
                    };

                    for (call, expected) in zip(params, param_tys) {
                        let call_ty = call.type_of(&self.type_map);
                        if !(call_ty == *expected) {
                            return Err(TypeError::ParameterTypeMismatch(
                                f_name.to_string(),
                                call_ty,
                                expected.clone(),
                            ));
                        }
                    }
                } else {
                    return Err(TypeError::NotDefined(f_name.to_string()));
                }
            }
            Identifier(i) => {
                if expr.type_of(&self.type_map) == T::Hole {
                    return Err(TypeError::NotDefined(i.to_string()));
                }
            }
            Literal(_l) => {} // literals encode their types themselves
            Infix {
                operation,
                lhs,
                rhs,
                ..
            } => {
                let lhs_ty = lhs.type_of(&self.type_map);
                let rhs_ty = rhs.type_of(&self.type_map);
                if lhs_ty != rhs_ty {
                    return Err(TypeError::InfixTypeMismatch(lhs_ty, rhs_ty));
                }

                use Bop::*;
                match operation {
                    Plus | Min | Div | Mul => {
                        if !lhs_ty.is_numeric() {
                            return Err(TypeError::NumericInfix(lhs_ty));
                        }
                    }
                    _ => todo!(),
                }
            }
            Assignment { binding, .. } => self.check_expr(binding)?, // TODO: check reassigns
            Return(e) => self.check_expr(e)?, // implicitly checked in any area that could be returning
            IfElse {
                cond,
                then_block,
                else_block,
            } => {
                let cond_ty = cond.type_of(&self.type_map);
                if cond_ty != T::BuiltIn(BuiltinType::Bool) {
                    return Err(TypeError::ConditionNotBoolean(cond_ty));
                }

                self.type_check(&then_block.instructions)?;
                if let Some(b) = else_block {
                    self.type_check(&b.instructions)?;
                }
            }
            Block(b) => self.type_check(&b.instructions)?,
            RecordDefinition(_) => {} // a record definition is always a valid type
        }

        Ok(())
    }

    pub fn type_check(&mut self, ast: &Vec<Expression>) -> Result<(), TypeError> {
        use Expression::*;
        // loop through the ast once to check if anything is introducing types
        // assignments are added to the type map, so when their identifier is used later on, we can
        // get its type
        // functions are added to ensure that function calls and return types are correct
        for expr in ast {
            match expr {
                Assignment { ident, binding } => {
                    let ty = binding.type_of(&self.type_map);
                    self.type_map.insert(ident.to_string(), ty);
                }
                Function(f) => {
                    let ty = f.type_of(&self.type_map);
                    self.type_map.insert(f.name.clone(), ty);
                }
                _ => {} // no other expressions can introduce a type
                        // TODO: when records and variants are added, they will need to be added here
            }
        }

        // ensure all expressions type check
        for expr in ast {
            self.check_expr(expr)?;
        }

        Ok(())
    }
}

#[cfg(test)]
mod tests {
    use super::{TypeChecker, TypeError};
    use crate::lex::tokenize;
    use crate::parse::{parse, BuiltinType, T};

    #[test]
    fn test_type_error() {
        let input = "let t = 2 + true;";
        let toks = tokenize(input);
        let ast = parse(toks).unwrap();

        let mut ty_checker = TypeChecker::new();
        let ty_result = ty_checker.type_check(&ast);
        assert_eq!(
            ty_result,
            Err(TypeError::InfixTypeMismatch(
                T::BuiltIn(BuiltinType::Int),
                T::BuiltIn(BuiltinType::Bool)
            ))
        );
    }
}
