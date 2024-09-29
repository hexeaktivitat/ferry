use miette::{Diagnostic, Result, SourceSpan};
use thiserror::Error;

use crate::{
    ir::FerryOpcode,
    state::{Convertable, FerryState, FerryValue},
};

#[derive(Diagnostic, Debug, Error)]
enum FerryVmError {
    #[error("Runtime errors")]
    RuntimeError {
        #[help]
        advice: String,
    },
}

type FerryResult<T> = Result<T, FerryVmError>;

pub struct FerryVm {
    instructions: Vec<FerryOpcode>,
    stack: Vec<FerryValue>,
    constants: Vec<i64>,
    pc: usize,
}

impl FerryVm {
    pub fn new(instructions: Vec<FerryOpcode>) -> Self {
        Self {
            instructions,
            stack: vec![],
            constants: vec![],
            pc: 0,
        }
    }

    pub fn interpret(&mut self, program: Vec<FerryOpcode>) -> FerryResult<FerryValue> {
        // self.program = program;
        self.run()
    }

    fn run(&mut self) -> FerryResult<FerryValue> {
        let mut result = FerryValue::Unit;

        for instruction in self.instructions.clone().iter() {
            match instruction {
                FerryOpcode::Nop => println!("nop"),
                FerryOpcode::Halt => println!("HALT!"),
                FerryOpcode::Return => result = self.stack.pop().unwrap(),
                FerryOpcode::Load(c) => self.stack.push(FerryValue::convert_from(*c)),
                FerryOpcode::Add => {
                    if self.stack.len() >= 2 {
                        let left = self.stack.pop().unwrap().convert_to();
                        let right = self.stack.pop().unwrap().convert_to();
                        let res: i64 = left + right;
                        self.stack.push(FerryValue::convert_from(res));
                    } else {
                        return Err(FerryVmError::RuntimeError {
                            advice: "Invalid addition".into(),
                        });
                    }
                }
                FerryOpcode::Sub => {
                    if self.stack.len() >= 2 {
                        let left = self.stack.pop().unwrap().convert_to();
                        let right = self.stack.pop().unwrap().convert_to();
                        let res: i64 = left - right;
                        self.stack.push(FerryValue::convert_from(res));
                    } else {
                        return Err(FerryVmError::RuntimeError {
                            advice: "Invalid subtraction".into(),
                        });
                    }
                }
                FerryOpcode::Mul => {
                    if self.stack.len() >= 2 {
                        let left = self.stack.pop().unwrap().convert_to();
                        let right = self.stack.pop().unwrap().convert_to();
                        let res: i64 = left * right;
                        self.stack.push(FerryValue::convert_from(res));
                    } else {
                        return Err(FerryVmError::RuntimeError {
                            advice: "Invalid multiplication".into(),
                        });
                    }
                }
                FerryOpcode::Div => {
                    if self.stack.len() >= 2 {
                        let left = self.stack.pop().unwrap().convert_to();
                        let right = self.stack.pop().unwrap().convert_to();
                        if right == 0 {
                            return Err(FerryVmError::RuntimeError {
                                advice: "DIVIDE BY ZERO".into(),
                            });
                        }
                        let res: i64 = left / right;
                        self.stack.push(FerryValue::convert_from(res));
                    } else {
                        return Err(FerryVmError::RuntimeError {
                            advice: "Invalid division".into(),
                        });
                    }
                }
            }
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

    #[test]
    fn check_load() {
        let mut vm = FerryVm::new(vec![FerryOpcode::Load(1), FerryOpcode::Load(2)]);
        vm.run().unwrap();
        assert!(vm.stack == vec![FerryValue::Number(1), FerryValue::Number(2)]);
    }

    #[test]
    fn check_add() {
        let mut vm = FerryVm::new(vec![
            FerryOpcode::Load(1),
            FerryOpcode::Load(2),
            FerryOpcode::Add,
            FerryOpcode::Return,
        ]);
        assert!(vm.run().unwrap() == FerryValue::Number(3));
    }
}
