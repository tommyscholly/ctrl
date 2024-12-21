use std::{
    collections::{HashMap, VecDeque},
    iter::Peekable,
    str::FromStr,
};

use core::slice::Iter;

use strum_macros::EnumString;

#[derive(Debug, Clone, PartialEq, Eq, EnumString)]
pub enum Token {
    Assign, // =

    // Primitives
    Id(String),
    Int(i64),
    Bool(bool),

    // Keywords
    Let,
    If,
    While,
    Else,
    And,
    Or,

    Return,
    Break,
    Continue,
    Unless,

    Not,
    Eql, // ==
    Ne,
    Le,
    Ge,
    Lt,
    Gt,

    #[strum(serialize = "+")]
    Plus,
    #[strum(serialize = "-")]
    Min,
    #[strum(serialize = "*")]
    Mul,
    #[strum(serialize = "/")]
    Div,

    #[strum(serialize = "{")]
    LBrace,
    #[strum(serialize = "}")]
    RBrace,

    #[strum(serialize = ";")]
    SemiColon,
}

// #[derive(Debug, Clone, Copy)]
// pub enum TokenStrErr {
//     NotRecognized,
// }

// This is a partial implementation
// impl FromStr for Token {
//     type Err = TokenStrErr;
//
//     fn from_str(s: &str) -> Result<Self, Self::Err> {
//         match s {
//             "+" => Ok(Token::Plus),
//             "-" => Ok(Token::Min),
//             "/" => Ok(Token::Div),
//             "*" => Ok(Token::Mul),
//             ";" => Ok(Token::SemiColon),
//             "{" => Ok(Token::LBrace),
//             "}" => Ok(Token::RBrace),
//             _ => Err(TokenStrErr::NotRecognized),
//         }
//     }
// }

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

fn take(token: Token, token_stream: &mut TokenStream<'_>, include_end: bool) -> Option<Vec<Token>> {
    let mut taken = Vec::new();
    while let Some(&tok) = token_stream.peek() {
        if tok != &token {
            taken.push(tok.clone());
            token_stream.next();
        } else {
            if include_end {
                token_stream.next(); // advance past the token we're looking for
                taken.push(tok.clone());
            }

            return Some(taken);
        }
    }

    // never found the token
    None
}

pub fn take_until(token: Token, token_stream: &mut TokenStream<'_>) -> Option<Vec<Token>> {
    take(token, token_stream, false)
}

pub fn take_through(token: Token, token_stream: &mut TokenStream<'_>) -> Option<Vec<Token>> {
    take(token, token_stream, true)
}

// assumes we're given something in the form of [Token::LBrace, ......, Token::RBrace]
pub fn take_block(token_stream: &mut TokenStream<'_>) -> Option<VecDeque<Token>> {
    let mut taken = VecDeque::new();
    let mut braces = 0;

    while let Some(&tok) = token_stream.peek() {
        if *tok != Token::RBrace {
            if *tok == Token::LBrace {
                braces += 1;
            }

            taken.push_back(tok.clone());
            token_stream.next();
        } else {
            token_stream.next(); // still advance past the token we're looking for
            taken.push_back(tok.clone());
            braces -= 1;
            if braces == 0 {
                return Some(taken);
            }
        }
    }

    None
}

pub fn tokenize(input: &str) -> Vec<Token> {
    let mut token_stream = Vec::new();

    let mut keywords = HashMap::from([
        ("true".to_string(), Token::Bool(true)),
        ("false".to_string(), Token::Bool(false)),
        ("let".to_string(), Token::Let),
        ("if".to_string(), Token::If),
        ("while".to_string(), Token::While),
        ("else".to_string(), Token::Else),
        ("and".to_string(), Token::And),
        ("return".to_string(), Token::Return),
        ("break".to_string(), Token::Break),
        ("continue".to_string(), Token::Continue),
        ("unless".to_string(), Token::Unless),
        ("or".to_string(), Token::Or),
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
                let mut n = c.to_string().parse::<i64>().expect("Impossible");
                input_stream.next();

                let mut next_digit = input_stream.peek();
                while let Some(&i) = next_digit {
                    if i.is_ascii_digit() {
                        let digit = i
                            .to_string()
                            .parse::<i64>()
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

            '+' | '-' | '/' | '*' | ';' | '{' | '}' => {
                input_stream.next();
                token_stream.push(Token::from_str(&c.to_string()).expect("to be able to from_str"))
            }

            '\n' => {
                line_no += 1;
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

    #[test]
    fn math_symbols() {
        let input = "+-/*";
        let tokens = tokenize(input);
        assert_eq!(
            tokens,
            vec![Token::Plus, Token::Min, Token::Div, Token::Mul]
        );
    }

    #[test]
    fn basic_ctrl_flow() {
        let input = "if true { return false; } else { return true; }";
        let tokens = tokenize(input);
        assert_eq!(
            tokens,
            vec![
                Token::If,
                Token::Bool(true),
                Token::LBrace,
                Token::Return,
                Token::Bool(false),
                Token::SemiColon,
                Token::RBrace,
                Token::Else,
                Token::LBrace,
                Token::Return,
                Token::Bool(true),
                Token::SemiColon,
                Token::RBrace,
            ]
        );
    }

    #[test]
    fn test_take_block() {
        let input = "if true { if true {1} else {0} } else {}";
        let output = "{ if true {1} else {0} }";

        let tokens = tokenize(input);
        let nested = Vec::from(&tokens[2..]);
        let mut iter = nested.iter().peekable();
        let block = take_block(&mut iter);
        assert!(block.is_some());
        assert_eq!(block.unwrap(), tokenize(output))
    }
}
