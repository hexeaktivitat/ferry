![ferry gbf fwee](img/Stamp57jp.png)

# ferry

A toy programming language for learning and exhibiting.

Long-term plans:
- [ ] LLVM backend for actual compilation (self-written RISC-V support is intended to be minimal and learning-focused)
- [ ] Typeclasses
- [ ] ???

## Building

`cargo run --release`

## Usage

Currently only features an interactive REPL for language testing. During the REPL session, you can invoke commands  with `!`. Recognized commands:

```
!tokens     # Prints the generated tokens of last expression
!state      # Prints the current REPL state
!ast        # Prints the Abstract Syntax Tree
!type       # Prints the Abstract Syntax Tree with TYPES
!asm        # Prints RISC-V ASM
!exit       # Exit REPL
!quit       # Quit REPL
```

Planned and expected features include:

- [ ] Flags to enable/disable diagnostic print as standard
  - May not implement; use case covered by below
- [x] Commands recognized by the REPL to print a diagnostic
- [ ] `ferry compile` subcommand to explicitly invoke a mock compilation process

## Language features

Extremely barebones at the moment.

Small roadmap:

- [x] Basic arithmetic operations `4 + 3 - 2 / 1`
  - [ ] Parenthetical grouping of expressions
- [x] Variable assignment `five = 5`
- [ ] Type assignment
  - Syntax options include: `Integer -> five = 5`, `five: Integer = 5` & `five := 5` for assigned vs inferred type
- [x] Booleans
- [x] `if`/`then`/`else` expressions
- [ ] loop structures `loop`/`while`/`for`
