use std::{collections::HashMap, iter::Peekable};

use core::slice::Iter;

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Token {
    Let,
    Assign,
    Id(String),
    Int(i32),
    Bool(bool),

    If,
    While,
    Else,
    And,
    Or,

    Not,
    Eql,
    Ne,
    Le,
    Ge,
    Lt,
    Gt,

    Plus,
    Min,
    Mul,
    Div,

    LBrace,
    RBrace,

    SemiColon,
}

pub type TokenStream<'a> = Peekable<Iter<'a, Token>>;

pub fn next_token_is_bop(token_stream: &mut TokenStream<'_>) -> bool {
    use Token::*;
    if let Some(&tok) = token_stream.peek() {
        (*tok == Eql || *tok == Ne || *tok == Le || *tok == Ge || *tok == Lt || *tok == Gt)
    } else {
        false
    }
}

pub fn validate_next_token(
    token: Token,
    mut token_stream: TokenStream<'_>,
) -> Result<TokenStream<'_>, TokenStream<'_>> {
    if let Some(&tok) = token_stream.peek() {
        if tok == &token {
            token_stream.next();
            Ok(token_stream)
        } else {
            Err(token_stream)
        }
    } else {
        Err(token_stream)
    }
}

pub fn take_until(token: Token, token_stream: &mut TokenStream<'_>) -> Option<Vec<Token>> {
    let mut taken = Vec::new();
    while let Some(&tok) = token_stream.peek() {
        if tok != &token {
            taken.push(tok.clone());
            token_stream.next();
        } else {
            token_stream.next(); // still advance past the token we're looking for
            taken.push(tok.clone());
            return Some(taken);
        }
    }

    // never found the token
    None
}

pub fn tokenize(input: &str) -> Vec<Token> {
    let mut token_stream = Vec::new();

    let mut keywords = HashMap::from([
        ("true".to_string(), Token::Bool(true)),
        ("false".to_string(), Token::Bool(false)),
        ("let".to_string(), Token::Let),
        ("if".to_string(), Token::If),
        ("else".to_string(), Token::Else),
        ("while".to_string(), Token::Else),
        ("and".to_string(), Token::And),
        ("or".to_string(), Token::Or),
        ("{".to_string(), Token::LBrace),
        ("}".to_string(), Token::RBrace),
    ]);

    let char_vec: Vec<char> = input.chars().collect();

    let mut line_no = 1;
    let mut input_stream = char_vec.into_iter().peekable();
    while let Some(&c) = input_stream.peek() {
        match c {
            ' ' | '\t' => {
                input_stream.next();
            }
            '0'..='9' => {
                let mut n = c.to_string().parse::<i32>().expect("Impossible");
                input_stream.next();

                let mut next_digit = input_stream.peek();
                while let Some(&i) = next_digit {
                    if i.is_ascii_digit() {
                        let digit = i
                            .to_string()
                            .parse::<i32>()
                            .expect("Character not a digit.");
                        n = n * 10 + digit;
                        input_stream.next();
                        next_digit = input_stream.peek();
                    } else {
                        next_digit = None;
                    }
                }
                token_stream.push(Token::Int(n));
            }
            '!' => {
                input_stream.next();
                if let Some(&cc) = input_stream.peek() {
                    if cc == '=' {
                        token_stream.push(Token::Ne);
                        input_stream.next();
                        continue;
                    }
                }
                token_stream.push(Token::Not);
            }
            '=' => {
                input_stream.next();
                if let Some(&cc) = input_stream.peek() {
                    if cc == '=' {
                        token_stream.push(Token::Eql);
                        input_stream.next();
                        continue;
                    }
                }
                token_stream.push(Token::Assign);
            }

            '+' => {
                input_stream.next();
                token_stream.push(Token::Plus);
            }
            '-' => {
                input_stream.next();
                token_stream.push(Token::Min);
            }
            '/' => {
                input_stream.next();
                token_stream.push(Token::Div);
            }
            '*' => {
                input_stream.next();
                token_stream.push(Token::Mul);
            }

            '\n' => {
                line_no += 1;
                input_stream.next();
            }
            ';' => {
                token_stream.push(Token::SemiColon);
                input_stream.next();
            }
            'A'..='Z' | 'a'..='z' | '_' => {
                let mut s = String::new();
                s.push(c);

                input_stream.next();
                let mut ch = input_stream.peek();
                while let Some(&c) = ch {
                    if c != '_' && !c.is_ascii_digit() && !c.is_alphabetic() {
                        ch = None;
                    } else {
                        s.push(c);
                        input_stream.next();
                        ch = input_stream.peek();
                    }
                }
                match keywords.get(&s) {
                    Some(t) => token_stream.push(t.clone()),
                    None => {
                        token_stream.push(Token::Id(s.clone()));
                        keywords.insert(s.clone(), Token::Id(s.clone()));
                    }
                }
            }
            _ => {
                println!("tok: {c}");
                unimplemented!();
            }
        }
    }

    token_stream
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn basic_stream() {
        let input = "let;";
        let tokens = tokenize(input);
        assert_eq!(tokens, vec![Token::Let, Token::SemiColon]);
    }

    #[test]
    fn basic_assignment_int() {
        let input = "let name = 5;";
        let tokens = tokenize(input);
        assert_eq!(
            tokens,
            vec![
                Token::Let,
                Token::Id(String::from("name")),
                Token::Assign,
                Token::Int(5),
                Token::SemiColon
            ]
        );
    }

    #[test]
    fn basic_assignment_bool_multi_line() {
        let input = "let name = true; let other_name = false;";
        let tokens = tokenize(input);
        assert_eq!(
            tokens,
            vec![
                Token::Let,
                Token::Id(String::from("name")),
                Token::Assign,
                Token::Bool(true),
                Token::SemiColon,
                Token::Let,
                Token::Id(String::from("other_name")),
                Token::Assign,
                Token::Bool(false),
                Token::SemiColon
            ]
        );
    }

    #[test]
    fn basic_eql() {
        let input = "let name = 5 == 5;";
        let tokens = tokenize(input);
        assert_eq!(
            tokens,
            vec![
                Token::Let,
                Token::Id(String::from("name")),
                Token::Assign,
                Token::Int(5),
                Token::Eql,
                Token::Int(5),
                Token::SemiColon
            ]
        );
    }

    #[test]
    fn basic_ne() {
        let input = "let name = 5 != 5;";
        let tokens = tokenize(input);
        assert_eq!(
            tokens,
            vec![
                Token::Let,
                Token::Id(String::from("name")),
                Token::Assign,
                Token::Int(5),
                Token::Ne,
                Token::Int(5),
                Token::SemiColon
            ]
        );
    }

    #[test]
    fn basic_not() {
        let input = "let name = !true;";
        let tokens = tokenize(input);
        assert_eq!(
            tokens,
            vec![
                Token::Let,
                Token::Id(String::from("name")),
                Token::Assign,
                Token::Not,
                Token::Bool(true),
                Token::SemiColon
            ]
        );
    }
}
