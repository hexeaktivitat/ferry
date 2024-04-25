use anyhow::Error;
use clap::{Parser, Subcommand};
use miette::Result;

use std::io::{stdin, stdout, Write};
use std::process::ExitCode;

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
}

fn main() -> ExitCode {
    let ferry_args = FerryArgs::parse();

    match ferry_args.command {
        Some(f) => match f {
            Commands::Compile { file } => {
                println!("{file}");
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
    print!("Fwee...> ");
    stdout().flush().expect("stdout didn't flush");
    let mut input = String::new();
    stdin()
        .read_line(&mut input)
        .expect("Unable to read from stdin");
    if input.trim_end() == "quit" || input.trim_end() == "exit" {
        println!("Exiting...");
        return Ok(());
    }
    let mut program = Ferry::new(input.clone());
    match program.run() {
        Ok(r) => println!("\n{}\n", r),
        Err(e) => eprintln!("\n{:?}\n", e),
    };
    loop {
        input = "".into();
        print!("Fwee...> ");
        stdout().flush().expect("stdout didn't flush");
        stdin().read_line(&mut input).expect("unable to read stdin");
        if input.trim_end() == "quit" || input.trim_end() == "exit" {
            println!("Invalid command; commands start with ! (eg. !quit !exit)");
        } else if input.starts_with("!") {
            match input.trim_end() {
                "!tokens" => program.print_data(PrintReq::Tokens),
                "!state" => program.print_data(PrintReq::State),
                "!ast" => program.print_data(PrintReq::Ast),
                "!type" => program.print_data(PrintReq::TypedAst),
                "!asm" => program.print_data(PrintReq::Asm),
                "!quit" | "!exit" => {
                    println!("Exiting...");
                    return Ok(());
                }
                _ => println!("Unrecognized special command {}", input),
            }
        } else {
            program.update_source(input.clone());
            match program.run() {
                Ok(r) => println!("\n{}\n", r),
                Err(e) => eprintln!("\n{:?}\n", e),
            };
        }
    }
}
