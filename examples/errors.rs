fn main() {
    // lexer error test
    lexer_test();
    // parser error test
    parser_test();
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

fn parser_test() {
    let code = std::fs::read_to_string("examples/parser_errors.feri").expect("file should exist");
    let mut program = ferry::Ferry::new(code);
    match program.run() {
        Ok(res) => println!("{res}"),
        Err(err) => eprintln!("{err:?}"),
    }
}
