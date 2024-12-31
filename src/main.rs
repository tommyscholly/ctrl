// #![allow(unused)]
// #![deny(clippy::all)] // Deny all clippy lints
// #![warn(clippy::pedantic)] // Warn for pedantic clippy lints
// #![allow(
//     clippy::module_name_repetitions,
//     clippy::similar_names,
//     clippy::enum_glob_use,
//     clippy::too_many_lines,
//     clippy::single_match_else,
//     clippy::needless_pass_by_value
// )] // Allow module name repetitions (optional)

use std::fs;

use clap::Parser;

use anyhow::Result;

mod cranelift;
mod lex;
mod parse;

#[derive(Parser)]
struct Cli {
    #[arg(short, long)]
    parse: bool,

    filename: String,
}

fn main() -> Result<()> {
    let cli = Cli::parse();

    let file_contents = fs::read_to_string(&cli.filename).unwrap();
    let lex = lex::tokenize(&file_contents);
    let ast = parse::parse(lex)?;

    if cli.parse {
        println!("{ast:?}");
    } else {
        let name_split = cli.filename.split('/').collect::<Vec<&str>>();
        let mod_name = name_split.last().unwrap();

        let compiler = cranelift::Compiler::new(mod_name);
        compiler.translate(ast)?;
    }

    Ok(())
}
