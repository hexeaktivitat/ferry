use miette::{Diagnostic, Result, SourceSpan};
use thiserror::Error;

use crate::{
    ir::FerryOpCode,
    state::{FerryState, FerryValue},
};

#[derive(Diagnostic, Debug, Error)]
enum FerryVmError {}

type FerryResult<T> = Result<T, FerryVmError>;

pub struct FerryVm {
    stack: Vec<FerryValue>,
    state: FerryState,
    program: Vec<FerryOpCode>,
    pc: usize,
}

impl FerryVm {
    pub fn new(state: &FerryState) -> Self {
        Self {
            stack: vec![],
            state: state.clone(),
            program: vec![],
            pc: 0,
        }
    }

    pub fn interpret(&mut self, program: Vec<FerryOpCode>) -> FerryResult<FerryValue> {
        self.program = program;
        self.run()
    }

    fn run(&mut self) -> FerryResult<FerryValue> {
        let mut result = FerryValue::Unit;
        let opcode = self.advance();

        Ok(result)
    }

    fn advance(&mut self) -> &FerryOpCode {
        let opcode = &self.program[self.pc];
        self.pc += 1;
        opcode
    }
}
