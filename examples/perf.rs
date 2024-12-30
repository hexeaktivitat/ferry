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
    println!("hello.feri: {} in: {elapsed_time}s", output.unwrap());
}

fn euler_1() {
    let source: String = fs::read_to_string("examples/euler1.feri").expect("file should exist");
    let mut program = ferry::Ferry::new(source);
    let start_time = Instant::now();
    let output = program.run();
    let elapsed_time = start_time.elapsed().as_secs_f64();
    println!("euler1.feri: {} in: {elapsed_time}s", output.unwrap());
}

fn fib() {
    let source: String = fs::read_to_string("examples/fib.feri").expect("file should exist");
    let mut program = ferry::Ferry::new(source);
    let start_time = Instant::now();
    let output = program.run();
    let elapsed_time = start_time.elapsed().as_secs_f64();
    println!("fib.feri: {} in: {elapsed_time}s", output.unwrap());
}
