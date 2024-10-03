use std::collections::HashMap;

use miette::{Diagnostic, Result};
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

struct FerryFrame {
    pub stack: Vec<FerryValue>,
    pub pc: usize,
    pub function: Vec<FerryOpcode>,
}

pub struct FerryVm {
    // instructions: Vec<FerryOpcode>,
    frames: Vec<FerryFrame>,
    heap: HashMap<FerryAddr, FerryValue>,
    // heap_ptr: u8,
    // constants: Vec<i64>,
    labels: HashMap<String, usize>,
    // pc: usize,
    fp: usize,
    // return pointer
    ret: Vec<usize>,
}

impl FerryVm {
    pub fn new() -> Self {
        Self {
            // instructions,
            frames: vec![],
            heap: HashMap::new(),
            // heap_ptr: 0x00,
            // constants: vec![],
            labels: HashMap::new(),
            // pc: 0,
            fp: 0,
            ret: vec![],
        }
    }

    // pub fn set_program(&mut self, instructions: Vec<FerryOpcode>) {
    //     self.frames[self.fp].pc = 0;
    //     // self.instructions = instructions;
    // }

    pub fn interpret(
        &mut self,
        instructions: Vec<FerryOpcode>,
        state: &mut FerryState,
    ) -> FerryResult<FerryValue> {
        // self.program = program;
        let result = self.run(instructions, state);
        if !self.frames[self.fp].stack.is_empty() {
            println!("STACK DID NOT CLEAR");
            self.frames[self.fp].stack = vec![];
        }
        result
    }

    fn run(
        &mut self,
        instructions: Vec<FerryOpcode>,
        state: &mut FerryState,
    ) -> FerryResult<FerryValue> {
        let mut result = FerryValue::Unit;
        loop {
            let instruction = self.advance(&instructions).clone();
            match instruction {
                FerryOpcode::Nop => println!("nop"),
                FerryOpcode::Halt => break,
                FerryOpcode::Return => {
                    let stack_val = self.frames[self.fp].stack.pop().unwrap();
                    if let FerryValue::Ptr(ptr) = stack_val {
                        result = self.heap.get(&ptr).unwrap().clone();
                    } else {
                        result = stack_val;
                    }
                }
                // FerryOpcode::Load => self.frames[self.fp].pop,
                FerryOpcode::LoadI(c) => {
                    self.frames[self.fp].stack.push(FerryValue::convert_from(c))
                }
                FerryOpcode::Alloc(ptr, a) => {
                    self.heap.insert(ptr, a.clone());
                    self.frames[self.fp].stack.push(FerryValue::Ptr(ptr));
                }
                FerryOpcode::Set(id) => {
                    // println!("{:?}", self.frames[self.fp].stack);
                    // println!("{id}");
                    let value = self.frames[self.fp].stack.last().unwrap();
                    state.add_symbol(&id, Some(value.clone()));
                }
                FerryOpcode::Get(id) => {
                    // println!("{:?}", self.frames[self.fp].stack);
                    // println!("{id}");
                    let value = state.get_symbol_value(&id).unwrap();
                    if let FerryValue::Ptr(ptr) = value {
                        self.frames[self.fp]
                            .stack
                            .push(self.heap.get(&ptr).unwrap().clone());
                    } else {
                        self.frames[self.fp].stack.push(value);
                    }
                }
                FerryOpcode::Pop => {
                    self.frames[self.fp].stack.pop().unwrap();
                }
                FerryOpcode::Add => {
                    if self.frames[self.fp].stack.len() >= 2 {
                        let right: i64 = self.frames[self.fp].stack.pop().unwrap().convert_to();
                        let left: i64 = self.frames[self.fp].stack.pop().unwrap().convert_to();
                        let res: i64 = left + right;
                        self.frames[self.fp]
                            .stack
                            .push(FerryValue::convert_from(res));
                    } else {
                        return Err(FerryVmError::RuntimeError {
                            advice: "Invalid addition".into(),
                        });
                    }
                }
                FerryOpcode::Sub => {
                    if self.frames[self.fp].stack.len() >= 2 {
                        let right: i64 = self.frames[self.fp].stack.pop().unwrap().convert_to();
                        let left: i64 = self.frames[self.fp].stack.pop().unwrap().convert_to();
                        let res: i64 = left - right;
                        self.frames[self.fp]
                            .stack
                            .push(FerryValue::convert_from(res));
                    } else {
                        return Err(FerryVmError::RuntimeError {
                            advice: "Invalid subtraction".into(),
                        });
                    }
                }
                FerryOpcode::Mul => {
                    if self.frames[self.fp].stack.len() >= 2 {
                        let right: i64 = self.frames[self.fp].stack.pop().unwrap().convert_to();
                        let left: i64 = self.frames[self.fp].stack.pop().unwrap().convert_to();
                        let res: i64 = left * right;
                        self.frames[self.fp]
                            .stack
                            .push(FerryValue::convert_from(res));
                    } else {
                        return Err(FerryVmError::RuntimeError {
                            advice: "Invalid multiplication".into(),
                        });
                    }
                }
                FerryOpcode::Div => {
                    if self.frames[self.fp].stack.len() >= 2 {
                        let right: i64 = self.frames[self.fp].stack.pop().unwrap().convert_to();
                        let left: i64 = self.frames[self.fp].stack.pop().unwrap().convert_to();
                        if right == 0 {
                            return Err(FerryVmError::RuntimeError {
                                advice: "DIVIDE BY ZERO".into(),
                            });
                        }
                        let res: i64 = left / right;
                        self.frames[self.fp]
                            .stack
                            .push(FerryValue::convert_from(res));
                    } else {
                        return Err(FerryVmError::RuntimeError {
                            advice: "Invalid division".into(),
                        });
                    }
                }
                FerryOpcode::And => todo!(),
                FerryOpcode::Or => todo!(),
                FerryOpcode::Not => {
                    if let FerryValue::Boolean(v) = self.frames[self.fp].stack.pop().unwrap() {
                        self.frames[self.fp].stack.push(FerryValue::Boolean(!v));
                    } else {
                        self.frames[self.fp].stack.push(FerryValue::Boolean(false));
                    }
                }
                FerryOpcode::Equal => {
                    let right: i64 = self.frames[self.fp].stack.pop().unwrap().convert_to();
                    let left: i64 = self.frames[self.fp].stack.pop().unwrap().convert_to();
                    let res = left == right;
                    self.frames[self.fp].stack.push(FerryValue::Boolean(res));
                }
                FerryOpcode::Greater => {
                    let right: i64 = self.frames[self.fp].stack.pop().unwrap().convert_to();
                    let left: i64 = self.frames[self.fp].stack.pop().unwrap().convert_to();
                    let res = left > right;
                    self.frames[self.fp].stack.push(FerryValue::Boolean(res));
                }
                FerryOpcode::Lesser => {
                    let right: i64 = self.frames[self.fp].stack.pop().unwrap().convert_to();
                    let left: i64 = self.frames[self.fp].stack.pop().unwrap().convert_to();
                    let res = left < right;
                    self.frames[self.fp].stack.push(FerryValue::Boolean(res));
                }
                FerryOpcode::Jump(offset) => {
                    self.frames[self.fp].pc += offset;
                }
                FerryOpcode::JumpCond(offset) => {
                    // let cond = self.frames[self.fp].stack.last().unwrap();
                    let cond = self.frames[self.fp].stack.pop().unwrap();
                    if !cond.truthiness() {
                        self.frames[self.fp].pc += offset;
                    }
                }
                FerryOpcode::JumpBack(offset) => {
                    self.frames[self.fp].pc -= offset;
                }
                FerryOpcode::Iter => {
                    let mut ptr_src = 0;
                    let iter: Vec<FerryValue> =
                        if let FerryValue::Ptr(ptr) = self.frames[self.fp].stack.pop().unwrap() {
                            ptr_src = ptr;
                            self.heap.get(&ptr).unwrap().clone()
                        } else {
                            FerryValue::List(vec![])
                        }
                        .convert_to();
                    let (head, tail) = iter.split_first().unwrap();
                    let tail_len = tail.len() as i64;
                    // push pointer back onto stack
                    self.frames[self.fp].stack.push(FerryValue::Ptr(ptr_src));
                    self.heap.insert(ptr_src, FerryValue::List(tail.into()));
                    // push len onto stack
                    self.frames[self.fp]
                        .stack
                        .push(FerryValue::Number(tail_len));
                    // push value of variable assignment
                    self.frames[self.fp].stack.push(head.clone());
                }
                FerryOpcode::Label(label) => {
                    self.labels.insert(label, self.frames[self.fp].pc);
                }
                FerryOpcode::JumpLabel(label) => {
                    // println!("pc: {}", self.frames[self.fp].pc);
                    self.ret.push(self.frames[self.fp].pc);
                    // println!("ret: {:?}", self.ret);
                    // println!("stack @ fn jump: {:?}", self.frames[self.fp].stack);
                    self.frames[self.fp].pc = state.get_label(&label).unwrap();
                }
                FerryOpcode::JumpRet => {
                    self.frames[self.fp].pc = self.ret.pop().unwrap();
                }
            }
        }
        Ok(result)
    }

    fn advance(&mut self, instructions: &[FerryOpcode]) -> FerryOpcode {
        let opcode = instructions[self.frames[self.fp].pc].clone();
        self.frames[self.fp].pc += 1;
        opcode
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn check_run() {
        let mut vm = FerryVm::new();
        let instructions = vec![FerryOpcode::Halt];
        assert!(vm.run(instructions, &mut FerryState::new()).unwrap() == FerryValue::Unit);
    }

    #[test]
    fn check_load() {
        let mut vm = FerryVm::new();
        let instructions = vec![
            FerryOpcode::LoadI(1),
            FerryOpcode::LoadI(2),
            FerryOpcode::Halt,
        ];
        vm.run(instructions, &mut FerryState::new()).unwrap();
        assert!(vm.frames[vm.fp].stack == vec![FerryValue::Number(1), FerryValue::Number(2)]);
    }

    #[test]
    fn check_add() {
        let mut vm = FerryVm::new();
        let instructions = vec![
            FerryOpcode::LoadI(1),
            FerryOpcode::LoadI(2),
            FerryOpcode::Add,
            FerryOpcode::Return,
            FerryOpcode::Halt,
        ];
        assert!(vm.run(instructions, &mut FerryState::new()).unwrap() == FerryValue::Number(3));
    }
}
