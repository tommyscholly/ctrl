use std::fs;

use clap::Parser;

mod lex;
mod parse;

#[derive(Parser)]
struct Cli {
    filename: String,
}

fn main() {
    let cli = Cli::parse();

    let file_contents = fs::read_to_string(&cli.filename).unwrap();
    let lex = lex::tokenize(&file_contents);
    match parse::parse(lex) {
        Ok(ast) => println!("{ast:?}"),
        Err(err) => println!("Parse Error: {err:?}"),
    }
}
