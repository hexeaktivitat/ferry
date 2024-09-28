use miette::{Diagnostic, Result, SourceSpan};
use thiserror::Error;

use crate::{
    ir::FerryOpcode,
    state::{FerryState, FerryValue},
};

#[derive(Diagnostic, Debug, Error)]
enum FerryVmError {}

type FerryResult<T> = Result<T, FerryVmError>;

pub struct FerryVm {
    instructions: Vec<FerryOpcode>,
    registers: [i32; 32],
    pc: usize,
}

impl FerryVm {
    pub fn new(instructions: Vec<FerryOpcode>) -> Self {
        Self {
            instructions,
            registers: [0; 32],
            pc: 0,
        }
    }

    pub fn interpret(&mut self, program: Vec<FerryOpcode>) -> FerryResult<FerryValue> {
        // self.program = program;
        self.run()
    }

    fn run(&mut self) -> FerryResult<FerryValue> {
        let mut result = FerryValue::Unit;
        match self.advance() {
            FerryOpcode::Nop => result = FerryValue::Unit,
            FerryOpcode::Halt => result = FerryValue::Unit,
            FerryOpcode::Load { dest, a } => todo!(),
            FerryOpcode::LoadI { dest, v } => todo!(),
            FerryOpcode::Add { dest, r1, r2 } => todo!(),
            FerryOpcode::Sub { dest, r1, r2 } => todo!(),
        }

        Ok(result)
    }

    fn advance(&mut self) -> &FerryOpcode {
        let opcode = &self.instructions[self.pc];
        self.pc += 1;
        opcode
    }
}

mod tests {
    use super::*;

    #[test]
    fn check_run() {
        let mut vm = FerryVm::new(vec![FerryOpcode::Halt]);
        assert!(vm.run().unwrap() == FerryValue::Unit);
    }
}
