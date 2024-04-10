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
            let mut program = Ferry::new("1 + 1".as_bytes());
            let result = program.run().unwrap();
            println!("{}", result);
        }
    }
}
