use std::io::{stdin, stdout, Write};

use clap::{Error, Parser, Subcommand};
use thiserror::Error;

use ferry::Ferry;

#[derive(Parser)]
#[clap(author, version, about, long_about = None)]
struct FerryArgs {
    file: Option<String>,
}

fn main() {
    let ferry_args = FerryArgs::parse();

    match ferry_args.file {
        Some(f) => println!("{f}"),
        None => {
            // run interpreter
            repl().unwrap();
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
    let mut result = program.run().unwrap();
    println!("\n{}", result);
    loop {
        input = "".into();
        print!("Fwee...> ");
        stdout().flush().expect("stdout didn't flush");
        stdin().read_line(&mut input).expect("unable to read stdin");
        if input.trim_end() == "quit" || input.trim_end() == "exit" {
            println!("Exiting...");
            return Ok(());
        }
        program.update_source(input.clone());
        result = program.run().unwrap();
        println!("\n{}\n", result);
    }
}
