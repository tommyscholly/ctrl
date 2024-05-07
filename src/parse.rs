#![allow(unused)]
use crate::lex::{take_until, validate_next_token, Token, TokenStream};

use core::slice::Iter;
use std::iter::Peekable;

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Ast {
    Assignment(String, Box<Ast>),
    Int(i32),
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

        let assignment_ast = parse(assignment);
        assert_eq!(assignment_ast.len(), 1);

        let ast = Ast::Assignment(name.to_string(), Box::new(assignment_ast[0].clone()));

        Ok((ast, token_stream))
    } else {
        panic!("let not followed by identifier");
    }
}

pub fn parse(tokens: Vec<Token>) -> Vec<Ast> {
    let mut ast = Vec::new();
    let mut token_stream = tokens.iter().peekable();

    while let Some(&tok) = token_stream.peek() {
        match tok {
            Token::Let => {
                token_stream.next();
                let let_expr_result = parse_let(token_stream);
                match let_expr_result {
                    Ok((let_expr, stream)) => {
                        token_stream = stream;
                        ast.push(let_expr);
                    }
                    Err(_) => panic!("let_expr did not bind correctly"),
                }
            }
            Token::Int(i) => {
                token_stream.next();
                ast.push(Ast::Int(*i));
            }
            _ => unreachable!(),
        }
    }

    ast
}

#[cfg(test)]
mod tests {
    use super::*;
    #[test]
    fn test_parse_let_binding() {
        let tokens = vec![
            Token::Let,
            Token::Id(String::from("name")),
            Token::Assign,
            Token::Int(5),
            Token::SemiColon,
        ];
        let ast = parse(tokens);
        println!("{:?}", ast);
        assert_eq!(
            ast,
            vec![Ast::Assignment("name".to_string(), Box::new(Ast::Int(5)))]
        )
    }
}
