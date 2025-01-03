use std::{
    collections::{HashMap, VecDeque},
    iter::Peekable,
    str::FromStr,
    string::ToString,
};

use core::slice::Iter;

use strum_macros::{Display, EnumString};

#[derive(Display, Debug, Clone, PartialEq, Eq, EnumString)]
pub enum Token {
    Assign, // =

    // Primitives
    #[strum(to_string = "Id({0})")]
    Id(String),
    #[strum(to_string = "Int({0})")]
    Int(i32),
    #[strum(to_string = "Bool({0})")]
    Bool(bool),

    // Keywords
    Let,
    If,
    While,
    Else,
    And,
    Or,
    Fn,

    Type,

    Return,
    Break,
    Continue,
    Unless,

    Not,
    Eql, // ==
    Neq,
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
    // cant use strum cause rust is annoying with }
    RBrace,

    #[strum(serialize = "(")]
    LParen,
    #[strum(serialize = ")")]
    RParen,

    #[strum(serialize = ";")]
    SemiColon,
    #[strum(serialize = ":")]
    Colon,

    #[strum(serialize = ",")]
    Comma,

    #[strum(serialize = "|")]
    Bar,
    #[strum(serialize = ".")]
    Dot,
}

pub type TokenStream<'a> = Peekable<Iter<'a, Token>>;

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
        if tok == &token {
            if include_end {
                token_stream.next(); // advance past the token we're looking for
                taken.push(tok.clone());
            }

            return Some(taken);
        }

        taken.push(tok.clone());
        token_stream.next();
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
        if *tok == Token::RBrace {
            token_stream.next(); // still advance past the token we're looking for
            taken.push_back(tok.clone());
            braces -= 1;
            if braces == 0 {
                return Some(taken);
            }
        } else {
            if *tok == Token::LBrace {
                braces += 1;
            }

            taken.push_back(tok.clone());
            token_stream.next();
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
        ("fn".to_string(), Token::Fn),
        ("type".to_string(), Token::Type),
    ]);

    let char_vec: Vec<char> = input.chars().collect();

    #[allow(unused)]
    let mut line_no = 1; // will use this when i start to add debug info
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
                        token_stream.push(Token::Neq);
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
            '<' => {
                input_stream.next();
                if let Some(&cc) = input_stream.peek() {
                    if cc == '=' {
                        token_stream.push(Token::Le);
                        input_stream.next();
                        continue;
                    }
                }
                token_stream.push(Token::Lt);
            }
            '>' => {
                input_stream.next();
                if let Some(&cc) = input_stream.peek() {
                    if cc == '=' {
                        token_stream.push(Token::Ge);
                        input_stream.next();
                        continue;
                    }
                }
                token_stream.push(Token::Gt);
            }

            '+' | '-' | '/' | '*' | ';' | '{' | '(' | ')' | ':' | ',' | '|' | '.' => {
                input_stream.next();
                token_stream.push(Token::from_str(&c.to_string()).expect("to be able to from_str"));
            }

            '}' => {
                input_stream.next();
                token_stream.push(Token::RBrace);
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
                Token::Neq,
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
        assert_eq!(block.unwrap(), tokenize(output));
    }

    #[test]
    fn test_fn_parse() {
        let input = "fn basic() {}";
        let tokens = tokenize(input);

        assert_eq!(
            tokens,
            vec![
                Token::Fn,
                Token::Id("basic".to_string()),
                Token::LParen,
                Token::RParen,
                Token::LBrace,
                Token::RBrace,
            ]
        );
    }

    #[test]
    fn test_fn_params() {
        let input = "fn params(x: int, y: bool) {}";
        let tokens = tokenize(input);

        assert_eq!(
            tokens,
            vec![
                Token::Fn,
                Token::Id("params".to_string()),
                Token::LParen,
                Token::Id("x".to_string()),
                Token::Colon,
                Token::Id("int".to_string()),
                Token::Comma,
                Token::Id("y".to_string()),
                Token::Colon,
                Token::Id("bool".to_string()),
                Token::RParen,
                Token::LBrace,
                Token::RBrace,
            ]
        );
    }

    #[test]
    fn test_fn_return_ty() {
        let input = "fn return_ty(): int { return 1; }";
        let tokens = tokenize(input);

        assert_eq!(
            tokens,
            vec![
                Token::Fn,
                Token::Id("return_ty".to_string()),
                Token::LParen,
                Token::RParen,
                Token::Colon,
                Token::Id("int".to_string()),
                Token::LBrace,
                Token::Return,
                Token::Int(1),
                Token::SemiColon,
                Token::RBrace,
            ]
        );
    }

    #[test]
    fn test_func_call() {
        let input = "test(1, thing)";
        let tokens = tokenize(input);

        assert_eq!(
            tokens,
            vec![
                Token::Id("test".to_string()),
                Token::LParen,
                Token::Int(1),
                Token::Comma,
                Token::Id("thing".to_string()),
                Token::RParen
            ]
        );
    }

    #[test]
    fn test_record_type() {
        let input = "type T = { x: int, y: bool }";
        let tokens = tokenize(input);

        assert_eq!(
            tokens,
            vec![
                Token::Type,
                Token::Id("T".to_string()),
                Token::Assign,
                Token::LBrace,
                Token::Id("x".to_string()),
                Token::Colon,
                Token::Id("int".to_string()),
                Token::Comma,
                Token::Id("y".to_string()),
                Token::Colon,
                Token::Id("bool".to_string()),
                Token::RBrace
            ]
        );
    }

    #[test]
    fn test_variant_type() {
        let input = "type T = One | Two";
        let tokens = tokenize(input);

        assert_eq!(
            tokens,
            vec![
                Token::Type,
                Token::Id("T".to_string()),
                Token::Assign,
                Token::Id("One".to_string()),
                Token::Bar,
                Token::Id("Two".to_string()),
            ]
        );
    }

    #[test]
    fn test_field_access() {
        let input = "t.x";
        let tokens = tokenize(input);

        assert_eq!(
            tokens,
            vec![
                Token::Id("t".to_string()),
                Token::Dot,
                Token::Id("x".to_string()),
            ]
        );
    }
}
