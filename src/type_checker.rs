use std::{
    collections::{HashMap, HashSet},
    iter::zip,
};

use strum_macros::Display;

use crate::parse::{Bop, BuiltinType, Expression, Record, Type, T};

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
    RecordNotDefined(String),
    RecordFieldMismatch,
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
            RecordInitialization(record_name, fields) => {
                let field_tys = match self.type_map.get(record_name) {
                    Some(T::Record(fields)) => fields,
                    _ => return Err(TypeError::RecordNotDefined(record_name.clone())),
                };

                if fields.len() != field_tys.len() {
                    return Err(TypeError::RecordFieldMismatch);
                }

                for ((expr_name, expr), (expected_name, expected_ty)) in zip(fields, field_tys) {
                    let expr_ty = expr.type_of(&self.type_map);
                    if expr_name != expected_name || expr_ty.final_ty() != expected_ty.final_ty() {
                        return Err(TypeError::RecordFieldMismatch);
                    }
                }
            }
        }

        Ok(())
    }

    // this is checking that 'subtype' <: 'supertype'
    // for instance, if we have a function that takes one parameter of type '{x: int}', and we're
    // passing a record of type '{x: int, y: int}', then subtype = '{x: int, y: int}', supertype = '{x: int}'
    // and 'subtype' <: 'supertype' is true
    // this checks both field names
    pub fn is_subtype(subtype: &Record, supertype: &Record) -> bool {
        // if supertype has more fields than the potential subtype, then it cannot possibly be valid
        let super_len = supertype.fields.len();
        let sub_len = supertype.fields.len();
        if super_len > sub_len {
            return false;
        }

        if super_len == sub_len {
            let matching = supertype
                .fields
                .iter()
                .zip(subtype.fields.iter())
                .filter(|&(a, b)| a == b)
                .count();

            return matching == super_len;
        }

        // then must be super_len < sub_len

        let mut sub_set = HashSet::new();
        for f in &subtype.fields {
            sub_set.insert(f);
        }
        let mut super_set = HashSet::new();
        for f in &supertype.fields {
            super_set.insert(f);
        }

        if super_set.is_subset(&sub_set) {
            return true;
        }

        false
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
                RecordDefinition(r) => {
                    let ty = r.type_of(&self.type_map);
                    self.type_map.insert(r.name.clone(), ty);
                }
                _ => {} // no other expressions can introduce a type
                        // TODO: variants are added, they will need to be added here
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
    use crate::parse::{parse, BuiltinType, Expression, Literal, Record, T};

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

    #[test]
    fn test_record_type_checking_ok() {
        let fields = vec![
            (String::from("x"), T::BuiltIn(BuiltinType::Int)),
            (String::from("y"), T::BuiltIn(BuiltinType::Bool)),
        ];
        let record = Record::new(String::from("T"), fields);
        let rd = Expression::RecordDefinition(record);

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
        let ri = Expression::RecordInitialization(String::from("T"), fields);

        let ast = vec![rd, ri];
        let mut ty_checker = TypeChecker::new();
        let ty_result = ty_checker.type_check(&ast);
        assert!(ty_result.is_ok());
    }

    #[test]
    fn test_record_type_checking_error() {
        let fields = vec![
            (String::from("x"), T::BuiltIn(BuiltinType::Int)),
            (String::from("y"), T::BuiltIn(BuiltinType::Bool)),
        ];
        let record = Record::new(String::from("T"), fields);
        let rd = Expression::RecordDefinition(record);

        let fields = vec![(
            String::from("z"),
            Box::new(Expression::Literal(Literal::Int(1))),
        )];
        let ri = Expression::RecordInitialization(String::from("T"), fields);

        let ast = vec![rd, ri];
        let mut ty_checker = TypeChecker::new();
        let ty_result = ty_checker.type_check(&ast);
        assert_eq!(ty_result, Err(TypeError::RecordFieldMismatch));
    }

    #[test]
    fn test_subtyping() {
        let fields_a = vec![
            (String::from("x"), T::BuiltIn(BuiltinType::Int)),
            (String::from("y"), T::BuiltIn(BuiltinType::Bool)),
        ];
        let record_a = Record::new(String::from("T"), fields_a);

        let fields_b = vec![(String::from("x"), T::BuiltIn(BuiltinType::Int))];
        let record_b = Record::new(String::from("T"), fields_b);

        let fields_c = vec![
            (String::from("x"), T::BuiltIn(BuiltinType::Int)),
            (String::from("y"), T::BuiltIn(BuiltinType::Int)),
        ];
        let record_c = Record::new(String::from("T"), fields_c);

        let fields_d = vec![
            (String::from("x"), T::BuiltIn(BuiltinType::Int)),
            (String::from("z"), T::BuiltIn(BuiltinType::Bool)),
        ];
        let record_d = Record::new(String::from("T"), fields_d);

        assert!(TypeChecker::is_subtype(&record_a, &record_b));
        assert!(TypeChecker::is_subtype(&record_a, &record_a));
        assert!(!TypeChecker::is_subtype(&record_b, &record_a));
        assert!(!TypeChecker::is_subtype(&record_a, &record_c));
        assert!(!TypeChecker::is_subtype(&record_a, &record_d));
    }
}
