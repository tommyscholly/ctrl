use std::fs;

use clap::Parser;

use anyhow::Result;

mod cranelift;
mod lex;
mod parse;

#[derive(Parser)]
struct Cli {
    filename: String,
}

fn main() -> Result<()> {
    let cli = Cli::parse();

    let file_contents = fs::read_to_string(&cli.filename).unwrap();
    let lex = lex::tokenize(&file_contents);
    let ast = parse::parse(lex)?;
    println!("{ast:?}");

    // cranelift::generate();

    Ok(())
}
