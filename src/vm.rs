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
    pub locals: HashMap<String, FerryValue>,
}

pub struct FerryVm {
    // instructions: Vec<FerryOpcode>,
    frames: Vec<FerryFrame>,
    heap: HashMap<FerryAddr, FerryValue>,
    // heap_ptr: u8,
    // constants: Vec<i64>,
    // locals: HashMap<String, FerryValue>,
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
            // locals: HashMap::new(),
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
        let frame = FerryFrame {
            stack: vec![],
            pc: 0,
            function: instructions,
            locals: HashMap::new(),
        };
        self.frames.push(frame);
        let result = self.run(state);
        if !self.frames[self.fp].stack.is_empty() {
            println!("STACK DID NOT CLEAR");
            self.frames[self.fp].stack = vec![];
        }
        result
    }

    fn run(&mut self, state: &mut FerryState) -> FerryResult<FerryValue> {
        let mut result = FerryValue::Unit;
        let mut add_count = 0;
        loop {
            let instruction = self.advance(self.frames[self.fp].function.clone());
            // if self.fp == 1 {
            //     println!("stack: {:?}", self.frames[self.fp].stack);
            //     println!("frame: {}", self.fp);
            //     println!("next op: {:?}", instruction);
            // }
            match instruction {
                FerryOpcode::Nop => println!("nop"),
                FerryOpcode::Halt => break,
                FerryOpcode::Return => {
                    let stack_val = match self.frames[self.fp].stack.pop() {
                        Some(v) => v,
                        None => FerryValue::Unit,
                    };
                    if let FerryValue::Ptr(ptr) = stack_val {
                        result = self.heap.get(&ptr).unwrap().clone();
                    } else {
                        result = stack_val;
                    }
                    if self.fp == 0 {
                        return Ok(result);
                    } else {
                        self.fp -= 1;
                        // println!("result : {:?}", result);
                        self.frames.pop();
                        self.frames[self.fp].stack.push(result.clone());
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
                    let value = self.frames[self.fp].stack.pop().unwrap();
                    self.frames[self.fp].locals.insert(id, value.clone());
                }
                FerryOpcode::Get(id) => {
                    // println!("{:?}", self.frames[self.fp].stack);
                    // println!("{id}");
                    let value = self.frames[self.fp]
                        .locals
                        .get(&id.clone())
                        .unwrap()
                        .clone();
                    if let FerryValue::Ptr(ptr) = value {
                        self.frames[self.fp]
                            .stack
                            .push(self.heap.get(&ptr).unwrap().clone());
                    } else {
                        self.frames[self.fp].stack.push(value.clone());
                    }
                }
                FerryOpcode::Pop => {
                    self.frames[self.fp].stack.pop().unwrap();
                }
                FerryOpcode::Add => {
                    add_count += 1;
                    // println!("adds: {add_count}");
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
                    // self.locals.insert(label, self.frames[self.fp].pc);
                    println!("lol");
                }
                FerryOpcode::Call(label) => {
                    self.ret.push(self.frames[self.fp].pc);
                    if let Some(FerryValue::Function {
                        declaration,
                        name,
                        func_type,
                        instructions,
                        arity,
                    }) = state.get_symbol_value(&label)
                    {
                        // println!("arity: {arity}");
                        let stack_len = self.frames[self.fp].stack.len();
                        let stack = self.frames[self.fp]
                            .stack
                            // .clone()
                            .split_off(stack_len - arity);
                        let mut frame = FerryFrame {
                            stack,
                            pc: 0,
                            function: instructions,
                            locals: HashMap::new(),
                        };

                        self.frames.push(frame);
                        self.fp += 1;
                        // println!("called: frame count: {}", self.fp);
                    }

                    // self.frames[self.fp].pc = state.get_label(&label).unwrap();
                }
                FerryOpcode::JumpRet => {
                    self.frames[self.fp].pc = self.ret.pop().unwrap();
                }
            }
        }
        Ok(result)
    }

    fn advance(&mut self, instructions: Vec<FerryOpcode>) -> FerryOpcode {
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
        assert!(vm.interpret(instructions, &mut FerryState::new()).unwrap() == FerryValue::Unit);
    }

    #[test]
    #[ignore]
    fn check_load() {
        let mut vm = FerryVm::new();
        let instructions = vec![
            FerryOpcode::LoadI(1),
            FerryOpcode::LoadI(2),
            FerryOpcode::Halt,
        ];
        vm.interpret(instructions, &mut FerryState::new()).unwrap();
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
        assert!(
            vm.interpret(instructions, &mut FerryState::new()).unwrap() == FerryValue::Number(3)
        );
    }
}
