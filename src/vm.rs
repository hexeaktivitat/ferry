use std::collections::HashMap;

use miette::{Diagnostic, Result, SourceSpan};
use thiserror::Error;

use crate::{
    ir::{FerryAddr, FerryOpcode},
    state::{Convertable, FerryState, FerryValue},
};

#[derive(Diagnostic, Debug, Error)]
pub enum FerryVmError {
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
    heap: HashMap<FerryAddr, FerryValue>,
    // heap_ptr: u8,
    constants: Vec<i64>,
    pc: usize,
}

impl FerryVm {
    pub fn new(instructions: Vec<FerryOpcode>) -> Self {
        Self {
            instructions,
            stack: vec![],
            heap: HashMap::new(),
            // heap_ptr: 0x00,
            constants: vec![],
            pc: 0,
        }
    }

    pub fn interpret(&mut self, state: &mut FerryState) -> FerryResult<FerryValue> {
        // self.program = program;
        self.run(state)
    }

    fn run(&mut self, state: &mut FerryState) -> FerryResult<FerryValue> {
        let mut result = FerryValue::Unit;
        loop {
            let instruction = self.advance().clone();
            match instruction {
                FerryOpcode::Nop => println!("nop"),
                FerryOpcode::Halt => break,
                FerryOpcode::Return => {
                    let stack_val = self.stack.pop().unwrap();
                    if let FerryValue::Ptr(ptr) = stack_val {
                        result = self.heap.get(&ptr).unwrap().clone();
                    } else {
                        result = stack_val;
                    }
                }
                FerryOpcode::Load(c) => self.stack.push(FerryValue::convert_from(c)),
                FerryOpcode::Alloc(ptr, a) => {
                    self.heap.insert(ptr, a.clone());
                }
                FerryOpcode::Set(id) => {
                    let value = self.stack.last().unwrap();
                    state.add_symbol(&id, Some(value.clone()));
                }
                FerryOpcode::Get(id) => {
                    let value = state.get_symbol_value(&id).unwrap();
                    self.stack.push(value);
                }
                FerryOpcode::Add => {
                    if self.stack.len() >= 2 {
                        let left: i64 = self.stack.pop().unwrap().convert_to();
                        let right: i64 = self.stack.pop().unwrap().convert_to();
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
                        let left: i64 = self.stack.pop().unwrap().convert_to();
                        let right: i64 = self.stack.pop().unwrap().convert_to();
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
                        let left: i64 = self.stack.pop().unwrap().convert_to();
                        let right: i64 = self.stack.pop().unwrap().convert_to();
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
                        let left: i64 = self.stack.pop().unwrap().convert_to();
                        let right: i64 = self.stack.pop().unwrap().convert_to();
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
                FerryOpcode::And => todo!(),
                FerryOpcode::Or => todo!(),
                FerryOpcode::Equality => todo!(),
                FerryOpcode::Jump(offset) => {
                    self.pc += offset;
                }
                FerryOpcode::JumpCond(offset) => {
                    let cond = self.stack.last().unwrap();
                    if !cond.truthiness() {
                        self.pc += offset;
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

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn check_run() {
        let mut vm = FerryVm::new(vec![FerryOpcode::Halt]);
        assert!(vm.run(&mut FerryState::new()).unwrap() == FerryValue::Unit);
    }

    #[test]
    fn check_load() {
        let mut vm = FerryVm::new(vec![
            FerryOpcode::Load(1),
            FerryOpcode::Load(2),
            FerryOpcode::Halt,
        ]);
        vm.run(&mut FerryState::new()).unwrap();
        assert!(vm.stack == vec![FerryValue::Number(1), FerryValue::Number(2)]);
    }

    #[test]
    fn check_add() {
        let mut vm = FerryVm::new(vec![
            FerryOpcode::Load(1),
            FerryOpcode::Load(2),
            FerryOpcode::Add,
            FerryOpcode::Return,
            FerryOpcode::Halt,
        ]);
        assert!(vm.run(&mut FerryState::new()).unwrap() == FerryValue::Number(3));
    }
}
