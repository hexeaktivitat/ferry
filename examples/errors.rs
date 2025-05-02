fn main() {
    // lexer error test
    lexer_test();
}

fn lexer_test() {
    let code = std::fs::read_to_string("examples/lexer_errors.feri").expect("file should exist");
    let mut program = ferry::Ferry::new(code);
    let res = program.run();
    match res {
        Ok(res) => println!("{res}"),
        Err(err) => eprintln!("{err:?}"),
    }
}
