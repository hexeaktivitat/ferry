![ferry gbf fwee](img/Stamp57jp.png)

# ferry

A toy programming language for learning and exhibiting.

## Building

`cargo run --release`

## Usage

During the REPL session, you can invoke commands to print compiler/interpreter states with `!`. Recognized commands:

```bash
!tokens
!state
!ast
!type
!asm
```

Currently only features an interactive REPL for language testing. Planned and expected features include:

- [ ] Flags to enable/disable diagnostic print as standard
- [x] Commands recognized by the REPL to print a diagnostic
- [ ] `ferry compile` subcommand to explicitly invoke a mock compilation process

## Language features

Extremely barebones at the moment.

Small roadmap:

- [x] Basic arithmetic operations `4 + 3 - 2 / 1`
- [x] Variable assignment `five = 5`
- [ ] Type assignment
  - Syntax options include: `Integer -> five = 5`, `five: Integer = 5` & `five := 5` for assigned vs inferred type
- [ ] Booleans
- [ ] `if`/`then`/`else` expressions
- [ ] loop structures `loop`/`while`/`for`
