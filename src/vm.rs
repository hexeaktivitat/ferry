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

pub struct FerryVm {
    instructions: Vec<FerryOpcode>,
    stack: Vec<FerryValue>,
    heap: HashMap<FerryAddr, FerryValue>,
    // heap_ptr: u8,
    // constants: Vec<i64>,
    labels: HashMap<String, usize>,
    pc: usize,
    fp: usize,
    // return pointer
    ret: Vec<usize>,
}

impl FerryVm {
    pub fn new(instructions: Vec<FerryOpcode>) -> Self {
        Self {
            instructions,
            stack: vec![],
            heap: HashMap::new(),
            // heap_ptr: 0x00,
            // constants: vec![],
            labels: HashMap::new(),
            pc: 0,
            fp: 0,
            ret: vec![],
        }
    }

    pub fn set_program(&mut self, instructions: Vec<FerryOpcode>) {
        self.pc = 0;
        self.instructions = instructions;
    }

    pub fn interpret(&mut self, state: &mut FerryState) -> FerryResult<FerryValue> {
        // self.program = program;
        self.run(state)
    }

    fn run(&mut self, state: &mut FerryState) -> FerryResult<FerryValue> {
        let mut result = FerryValue::Unit;
        loop {
            let instruction = self.advance().clone();
            println!(
                "inst: {:?} \n pc: {} \n stack: {:?} \n ret: {:?} \n state: {}",
                instruction, self.pc, self.stack, self.ret, state
            );
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
                FerryOpcode::Load => todo!(),
                FerryOpcode::LoadI(c) => self.stack.push(FerryValue::convert_from(c)),
                FerryOpcode::Alloc(ptr, a) => {
                    self.heap.insert(ptr, a.clone());
                    self.stack.push(FerryValue::Ptr(ptr));
                }
                FerryOpcode::Set(id) => {
                    // println!("{:?}", self.stack);
                    // println!("{id}");
                    let value = self.stack.last().unwrap();
                    state.add_symbol(&id, Some(value.clone()));
                }
                FerryOpcode::Get(id) => {
                    // println!("{:?}", self.stack);
                    // println!("{id}");
                    let value = state.get_symbol_value(&id).unwrap();
                    if let FerryValue::Ptr(ptr) = value {
                        self.stack.push(self.heap.get(&ptr).unwrap().clone());
                    } else {
                        self.stack.push(value);
                    }
                }
                FerryOpcode::Pop => {
                    self.stack.pop().unwrap();
                }
                FerryOpcode::Add => {
                    if self.stack.len() >= 2 {
                        let right: i64 = self.stack.pop().unwrap().convert_to();
                        let left: i64 = self.stack.pop().unwrap().convert_to();
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
                        let right: i64 = self.stack.pop().unwrap().convert_to();
                        let left: i64 = self.stack.pop().unwrap().convert_to();
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
                        let right: i64 = self.stack.pop().unwrap().convert_to();
                        let left: i64 = self.stack.pop().unwrap().convert_to();
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
                        let right: i64 = self.stack.pop().unwrap().convert_to();
                        let left: i64 = self.stack.pop().unwrap().convert_to();
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
                FerryOpcode::Not => {
                    if let FerryValue::Boolean(v) = self.stack.pop().unwrap() {
                        self.stack.push(FerryValue::Boolean(!v));
                    } else {
                        self.stack.push(FerryValue::Boolean(false));
                    }
                }
                FerryOpcode::Equal => {
                    let right: i64 = self.stack.pop().unwrap().convert_to();
                    let left: i64 = self.stack.pop().unwrap().convert_to();
                    let res = left == right;
                    self.stack.push(FerryValue::Boolean(res));
                }
                FerryOpcode::Greater => {
                    let right: i64 = self.stack.pop().unwrap().convert_to();
                    let left: i64 = self.stack.pop().unwrap().convert_to();
                    let res = left > right;
                    self.stack.push(FerryValue::Boolean(res));
                }
                FerryOpcode::Lesser => {
                    let right: i64 = self.stack.pop().unwrap().convert_to();
                    let left: i64 = self.stack.pop().unwrap().convert_to();
                    let res = left < right;
                    self.stack.push(FerryValue::Boolean(res));
                }
                FerryOpcode::Jump(offset) => {
                    self.pc += offset;
                }
                FerryOpcode::JumpCond(offset) => {
                    // let cond = self.stack.last().unwrap();
                    let cond = self.stack.pop().unwrap();
                    if !cond.truthiness() {
                        self.pc += offset;
                    }
                }
                FerryOpcode::JumpBack(offset) => {
                    self.pc -= offset;
                }
                FerryOpcode::Iter => {
                    let mut ptr_src = 0;
                    let iter: Vec<FerryValue> =
                        if let FerryValue::Ptr(ptr) = self.stack.pop().unwrap() {
                            ptr_src = ptr;
                            self.heap.get(&ptr).unwrap().clone()
                        } else {
                            FerryValue::List(vec![])
                        }
                        .convert_to();
                    let (head, tail) = iter.split_first().unwrap();
                    let tail_len = tail.len() as i64;
                    // push pointer back onto stack
                    self.stack.push(FerryValue::Ptr(ptr_src));
                    self.heap.insert(ptr_src, FerryValue::List(tail.into()));
                    // push len onto stack
                    self.stack.push(FerryValue::Number(tail_len));
                    // push value of variable assignment
                    self.stack.push(head.clone());
                }
                FerryOpcode::Label(label) => {
                    self.labels.insert(label, self.pc);
                }
                FerryOpcode::JumpLabel(label) => {
                    // println!("pc: {}", self.pc);
                    self.ret.push(self.pc);
                    // println!("ret: {:?}", self.ret);
                    // println!("stack @ fn jump: {:?}", self.stack);
                    self.pc = state.get_label(&label).unwrap();
                }
                FerryOpcode::JumpRet => {
                    self.pc = self.ret.pop().unwrap();
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
            FerryOpcode::LoadI(1),
            FerryOpcode::LoadI(2),
            FerryOpcode::Halt,
        ]);
        vm.run(&mut FerryState::new()).unwrap();
        assert!(vm.stack == vec![FerryValue::Number(1), FerryValue::Number(2)]);
    }

    #[test]
    fn check_add() {
        let mut vm = FerryVm::new(vec![
            FerryOpcode::LoadI(1),
            FerryOpcode::LoadI(2),
            FerryOpcode::Add,
            FerryOpcode::Return,
            FerryOpcode::Halt,
        ]);
        assert!(vm.run(&mut FerryState::new()).unwrap() == FerryValue::Number(3));
    }
}
