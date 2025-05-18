// simple manual performance checks

use std::{fs, time::Instant};

fn main() {
    for _ in 1..25 {
        hello();
    }
    for _ in 1..25 {
        euler_1();
    }
    for _ in 1..25 {
        fib();
    }
}

// #[expect(unused)]
fn hello() {
    let source: String =
        fs::read_to_string("examples/hello_ferry.feri").expect("file should exist");
    let mut program = ferry::Ferry::new(source);
    let start_time = Instant::now();
    let output = program.run();
    let elapsed_time = start_time.elapsed().as_secs_f64();
    match output {
        Ok(o) => println!("hello.feri: {o} in: {elapsed_time}s"),
        Err(e) => eprintln!("Errors encountered: {e:?}"),
    }
}

fn euler_1() {
    let source: String = fs::read_to_string("examples/euler1.feri").expect("file should exist");
    let mut program = ferry::Ferry::new(source);
    let start_time = Instant::now();
    let output = program.run();
    let elapsed_time = start_time.elapsed().as_secs_f64();
    match output {
        Ok(o) => println!("euler1.feri: {o} in: {elapsed_time}s"),
        Err(e) => eprintln!("Errors encountered: {e:?}"),
    }
}

fn fib() {
    let source: String = fs::read_to_string("examples/fib.feri").expect("file should exist");
    let mut program = ferry::Ferry::new(source);
    let start_time = Instant::now();
    let output = program.run();
    let elapsed_time = start_time.elapsed().as_secs_f64();
    match output {
        Ok(o) => println!("fib.feri: {o} in: {elapsed_time}s"),
        Err(e) => eprintln!("Errors encountered: {e:?}"),
    }
}
