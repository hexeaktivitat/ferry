// Euler problem 1
#[test]
fn euler_1() {
    let source: String = "
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

    let mut program = ferry::Ferry::new(source);
    assert_eq!(program.run().unwrap().to_string(), "234168");
}

// nth Fibonacci number
#[test]
fn fib() {
    let source: String = "
    def fn fib(n: Int) -> Int: (
        if n <= 1 then:
            n
        else: (fib(n - 2) + fib(n - 1))
    )

    fib(20)"
        .into();

    let mut program = ferry::Ferry::new(source);
    assert_eq!(program.run().unwrap().to_string(), "6765");
}
