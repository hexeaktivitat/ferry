use anyhow::Error;
use clap::{Parser, Subcommand};
use miette::Result;

use std::{
    fs::read_to_string,
    io::{stdin, stdout, Write},
    process::ExitCode,
};

use ferry::{Ferry, PrintReq};

#[derive(Parser)]
#[clap(author, version, about, long_about = None)]
struct FerryArgs {
    #[command(subcommand)]
    command: Option<Commands>,
}

#[derive(Subcommand)]
enum Commands {
    Compile { file: String },
    Run { file: String },
}

fn main() -> ExitCode {
    let ferry_args = FerryArgs::parse();

    match ferry_args.command {
        Some(f) => match f {
            Commands::Compile { file } => {
                println!("{file}");
                ExitCode::SUCCESS
            }
            Commands::Run { file } => {
                let source_code = read_to_string(file).expect("couldn't read from file");
                let mut program = Ferry::new(source_code.clone());
                match program.run() {
                    Ok(r) => println!("{r}"),
                    Err(e) => eprintln!("{:?}", e),
                }
                // program.print_data(PrintReq::TypedAst);
                ExitCode::SUCCESS
            }
        },
        None => {
            // run interpreter
            match repl() {
                Ok(_) => ExitCode::SUCCESS,
                Err(e) => {
                    println!("Error: {}", e);
                    ExitCode::FAILURE
                }
            }
        }
    }
}

fn repl() -> Result<(), Error> {
    let mut input = String::new();
    let mut program = Ferry::new(input.clone());

    println!("\nFERRY 0.2.3");
    println!("===========");
    println!("Language Design Experiment");
    println!("Type !help for commands, or !quit to quit\n");

    loop {
        print!("Fwee...> ");
        stdout().flush().expect("stdout didn't flush");

        input = "".into();
        stdin()
            .read_line(&mut input)
            .expect("Unable to read from stdin");

        match repl_input_process(&input) {
            Some(r) => match r {
                FerryRepl::Run(code) => {
                    program.update_source(code);
                    match program.run() {
                        Ok(r) => println!("\n{}\n", r),
                        Err(e) => eprintln!("\n{:?}\n", e),
                    }
                }
                FerryRepl::Exit => {
                    println!("Exiting...");
                    return Ok(());
                }
                FerryRepl::Tokens => program.print_data(PrintReq::Tokens),
                FerryRepl::State => program.print_data(PrintReq::State),
                FerryRepl::Ast => program.print_data(PrintReq::Ast),
                FerryRepl::Type => program.print_data(PrintReq::TypedAst),
                FerryRepl::Asm => program.print_data(PrintReq::Asm),
                FerryRepl::Help => print_help(),
            },
            None => {
                println!("invalid command {input}");
                continue;
            }
        }
    }
}

enum FerryRepl {
    Run(String),
    Exit,
    Tokens,
    State,
    Ast,
    Type,
    Asm,
    Help,
}

fn repl_input_process(input: &str) -> Option<FerryRepl> {
    let output = input.trim_end();

    if output.starts_with('!') {
        match output.trim_start_matches('!').to_lowercase().as_str() {
            "exit" | "quit" => Some(FerryRepl::Exit),
            "token" | "tokens" => Some(FerryRepl::Tokens),
            "state" => Some(FerryRepl::State),
            "ast" => Some(FerryRepl::Ast),
            "type" => Some(FerryRepl::Type),
            "asm" => Some(FerryRepl::Asm),
            "help" => Some(FerryRepl::Help),
            _ => None,
        }
    } else {
        Some(FerryRepl::Run(output.into()))
    }
}

fn print_help() {
    println!("\nList of Ferry commands:");
    println!("=======================\n");
    println!("!token: Print list of source code tokens for last command");
    println!("!ast:   Print a representation of the AST for the last comand");
    println!("!type:  Print a representation of the AST after the typechecker");
    println!("!state: Print the current global state (variables, etc)");
    println!("!asm:   Print the generated RISC-V assembly (alpha)");
    println!("!exit:");
    println!("!quit:  Quit the REPL");
    println!();
}
