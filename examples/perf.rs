// simple manual performance checks

use std::time::Instant;

fn main() {
    euler_1();
    fib();
}

fn euler_1() {
    let euler_1: String = "
        def fn abs(a: Int) -> Int: (
            if (a < 0) then: a * (0 - 1)
            else: a
        )

        def fn modulo(a: Int, b: Int) -> Int: (
            if (a - b) < b then: (
                abs(a - b)
            ) else: (
                modulo(abs(a-b), b)
            )
        )

        // modulo operator needed for this

        let sum: Int = 0

        for n: Int in 1..1000: 
            if modulo(n, 3) == 0 then: 
                sum = sum + n
            else: (
                if modulo(n, 5) == 0 then: 
                    sum = sum + n
            )

        sum"
    .into();

    let mut program = ferry::Ferry::new(euler_1);

    let start_time = Instant::now();

    let output = program.run();

    let elapsed_time = start_time.elapsed().as_secs_f64();

    println!("{} in: {elapsed_time}s", output.unwrap());
}

fn fib() {
    let source: String = "
        def fn fib(n: Int) -> Int: (
            if n <= 1 then:
                n
            else: (fib(n - 2) + fib(n - 1))
        )

        fib(30)"
        .into();

    let mut program = ferry::Ferry::new(source);

    let start_time = Instant::now();

    let output = program.run();

    let elapsed_time = start_time.elapsed().as_secs_f64();

    println!("{} in: {elapsed_time}s", output.unwrap());
}
