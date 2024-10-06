// Euler problem 1
#[test]
// #[ignore]
fn euler_1() {
    let source: String = "
            def fn modulo(a: Int, b: Int) -> Int: (
                if (a < b) then: (
                    a
                ) else : (
                    modulo((a - b), b)
                )
            )

            // modulo operator needed for this

            let sum: Int = 0

            for n: Int in 1..10: 
                if modulo(n, 3) == 0 then: 
                    sum = sum + n
                else: (
                    if modulo(n, 5) == 0 then: 
                        sum = sum + n
                )

            sum
        "
    .into();

    let mut program = ferry::Ferry::new(source);
    assert_eq!(program.run().unwrap().to_string(), "33");
}

// nth Fibonacci number
#[test]
fn fib() {
    let source: String = "
    def fn fib(n: Int) -> Int: (
        if n <= 1 then:
            n
        else: (fib(n - 1) + fib(n - 2))
    )

    fib(20)"
        .into();

    let mut program = ferry::Ferry::new(source);
    assert_eq!(program.run().unwrap().to_string(), "6765");
}
