use std::io::{stdin, stdout, Write};

use clap::{Args, Parser, Subcommand};

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
            repl();
        }
    }
}

fn repl() {
    print!("Fwee...> ");
    stdout().flush().expect("stdout didn't flush");
    let mut input = String::new();
    stdin()
        .read_line(&mut input)
        .expect("Unable to read from stdin");
    if input == "quit\n" || input == "exit\n" {
        println!("Exiting...");
        return;
    }
    let mut program = Ferry::new(input.clone());
    let mut result = program.run().unwrap();
    println!("{}", result);
    loop {
        input = "".into();
        print!("Fwee...> ");
        stdout().flush().expect("stdout didn't flush");
        stdin().read_line(&mut input).expect("unable to read stdin");
        if input == "quit\n" || input == "exit\n" {
            println!("Exiting...");
            return;
        }
        program.update_source(input.clone());
        result = program.run().unwrap();
        println!("{}", result);
    }
}
