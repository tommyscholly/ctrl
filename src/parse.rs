#![allow(unused)]
use crate::lex::{next_token_is_bop, take_until, validate_next_token, Token, TokenStream};

use core::slice::Iter;
use std::iter::Peekable;

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Bop {
    Eql,
    Le,
    Lt,
    Gt,
    Ge,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Ast {
    Assignment(String, Box<Ast>),
    Int(i32),
    Bool(bool),
    BinOp {
        lhs: Box<Ast>,
        bop: Bop,
        rhs: Box<Ast>,
    },
}

pub fn parse_let(
    mut token_stream: TokenStream<'_>,
) -> Result<(Ast, TokenStream<'_>), TokenStream<'_>> {
    if let &Token::Id(name) = token_stream
        .peek()
        .expect("let should be followed by an identifier")
    {
        token_stream.next();
        match validate_next_token(Token::Assign, token_stream) {
            Ok(stream) => token_stream = stream,
            Err(stream) => return Err(stream),
        }

        let assignment = match take_until(Token::SemiColon, &mut token_stream) {
            Some(a) => a,
            None => return Err(token_stream),
        };

        let mut assignment_ast = parse(assignment);
        assert_eq!(assignment_ast.len(), 1);

        // safe to pop because one elem
        let ast = Ast::Assignment(name.to_string(), Box::new(assignment_ast.pop().unwrap()));

        Ok((ast, token_stream))
    } else {
        panic!("let not followed by identifier");
    }
}

pub fn parse_bop(
    lhs: Ast,
    bop: Bop,
    mut token_stream: TokenStream<'_>,
) -> Result<(Ast, TokenStream<'_>), TokenStream<'_>> {
    let rhs_tokens = match take_until(Token::SemiColon, &mut token_stream) {
        Some(a) => a,
        None => return Err(token_stream),
    };

    let mut rhs_ast = parse(rhs_tokens);
    assert_eq!(rhs_ast.len(), 1);

    let ast = Ast::BinOp {
        lhs: Box::new(lhs),
        bop,
        // safe to pop because one elem
        rhs: Box::new(rhs_ast.pop().unwrap()),
    };

    Ok((ast, token_stream))
}

pub fn parse(tokens: Vec<Token>) -> Vec<Ast> {
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
                    Err(_) => panic!("let_expr did not bind correctly"),
                }
            }
            Token::Int(i) => ast.push(Ast::Int(*i)),
            Token::Bool(b) => ast.push(Ast::Bool(*b)),
            Token::Eql => {
                let lhs = ast
                    .pop()
                    .expect("there should be a left hand side to a binary operation");
                let bop_expr_result = parse_bop(lhs, Bop::Eql, token_stream);
                match bop_expr_result {
                    Ok((bop_expr, stream)) => {
                        token_stream = stream;
                        ast.push(bop_expr);
                    }
                    Err(_) => panic!("bop_expr did not bind correctly"),
                }
            }
            Token::SemiColon => {}
            _ => unreachable!(),
        }
    }

    ast
}

#[cfg(test)]
mod tests {
    use super::*;
    #[test]
    fn test_parse_let_int_binding() {
        let tokens = vec![
            Token::Let,
            Token::Id(String::from("name")),
            Token::Assign,
            Token::Int(5),
            Token::SemiColon,
        ];
        let ast = parse(tokens);
        assert_eq!(
            ast,
            vec![Ast::Assignment("name".to_string(), Box::new(Ast::Int(5)))]
        )
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
        let ast = parse(tokens);
        assert_eq!(
            ast,
            vec![Ast::Assignment(
                "name".to_string(),
                Box::new(Ast::Bool(true))
            )]
        )
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
        let ast = parse(tokens);
        println!("{:?}", ast);
        assert_eq!(
            ast,
            vec![Ast::Assignment(
                "name".to_string(),
                Box::new(Ast::BinOp {
                    lhs: Box::new(Ast::Int(5)),
                    bop: Bop::Eql,
                    rhs: Box::new(Ast::Int(5)),
                }),
            )]
        )
    }
}
