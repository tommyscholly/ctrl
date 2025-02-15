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

use std::{collections::HashMap, fs};

use clap::Parser;

use anyhow::Result;
// use type_checker::TypeChecker;

// mod cranelift;
mod ir;
mod lex;
mod parse;
// mod type_checker;

use ir::TypedIR;

#[derive(Parser)]
struct Cli {
    #[arg(short, long)]
    parse: bool,
    #[arg(short, long)]
    ir: bool,
    #[arg(short, long)]
    type_check: bool, // just type check and return

    filename: String,
}

fn main() -> Result<()> {
    let cli = Cli::parse();

    let file_contents = fs::read_to_string(&cli.filename).unwrap();
    let lex = lex::tokenize(&file_contents);
    let ast = parse::parse(lex)?;
    // let mut ty_checker = TypeChecker::new();
    // let ty_results = ty_checker.type_check(&ast);
    // if let Err(t) = ty_results {
    //     println!("Type Error: {t}");
    //     return Ok(());
    // } else {
    //     println!("Type Map: {:?}\n", ty_checker.type_map);
    // }
    //

    if cli.parse {
        println!("{ast:?}");
        return Ok(());
    }

    let mut type_info = HashMap::new();
    let mut typed_ir = vec![];
    for expr in ast {
        let ty_expr = TypedIR::new(expr, &mut type_info);
        typed_ir.push(ty_expr);
    }

    if cli.type_check {
        println!("{typed_ir:?}");
        return Ok(());
    }

    let name_split = cli.filename.split('/').collect::<Vec<&str>>();
    #[allow(unused)]
    let mod_name = name_split.last().unwrap();

    // let compiler = cranelift::Compiler::new(mod_name, cli.ir, &ty_checker.type_map);
    // compiler.translate(ast)?;

    Ok(())
}
