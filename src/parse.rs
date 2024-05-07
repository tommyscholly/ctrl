#![allow(unused)]
use crate::lex::Token;

use core::slice::Iter;
use std::iter::Peekable;

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Ast {
    Assignment(String, Box<Ast>),
}

pub fn parse_let(mut token_stream: Peekable<Iter<Token>>) -> Ast {
    if let Some(&Token::Id(name)) = token_stream
        .peek()
        .expect("let should be followed by an identifier")
    {
        let assignment: Vec<&Token> = token_stream
            .take_while(|&tok| tok != &Token::SemiColon)
            .collect();
    } else {
        panic!("let not followed by identifier");
    }
}

pub fn parse(tokens: Vec<Token>) -> Vec<Ast> {
    let mut ast = Vec::new();
    let mut token_stream = tokens.iter().peekable();

    while let Some(&tok) = token_stream.peek() {
        match tok {
            Token::Let => {}
            _ => unreachable!(),
        }
    }

    ast
}
