![ferry gbf fwee](img/Stamp57jp.png)

![build](https://github.com/hexeaktivitat/ferry/actions/workflows/rust.yml/badge.svg)
![GitHub Release](https://img.shields.io/github/v/release/hexeaktivitat/ferry?sort=semver)

# ferry

Ferry is an expression-oriented interpreted functional programming language developed with the aim of being an educational language, both for learning programming as a beginner and for learning how compilers are structured and function. Ferry is meant to be a simple and straightforward language, and while it should be a capable language, producing efficient production-ready code is not a goal of the project.

## Building

Clone the repository and then `cargo run --release` to run the interpreter. You may also install the application to cargo's `$PATH` by using `cargo install --path=.`.

## Usage

### REPL

Invoking ferry with `ferry` starts the interactive REPL. During the REPL session, you can invoke commands  with `!`. Recognized commands:

```bash
!tokens     # Prints the generated tokens of last expression
!state      # Prints the current REPL state
!ast        # Prints the Abstract Syntax Tree
!type       # Prints the Abstract Syntax Tree with TYPES
!asm        # Prints RISC-V ASM (alpha, has not been fully implemented yet)
!exit       # Exit REPL
!quit       # Quit REPL
```

### Source code interpretation

If you have source code in a stand-alone `.feri` file, you can run it through the interpreter with `ferry run [FILE]`. The return value of the program will be its last expression evaluated.
