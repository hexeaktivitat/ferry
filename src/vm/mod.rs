use std::{cell::RefCell, collections::HashMap, rc::Rc};

use miette::{Diagnostic, Result};
use thiserror::Error;

use crate::{
    ir::{FerryAddr, Opcode},
    state::{value::Value, State},
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

#[derive(Debug)]
struct Frame {
    pub stack: Vec<Value>,
    pub pc: usize,
    pub function: Rc<RefCell<Vec<Opcode>>>,
    pub locals: HashMap<String, Value>,
}

pub struct Vm {
    // instructions: Vec<FerryOpcode>,
    frames: Vec<Frame>,
    heap: HashMap<FerryAddr, Value>,
    // heap_ptr: u8,
    // constants: Vec<i64>,
    // locals: HashMap<String, FerryValue>,
    // pc: usize,
    fp: usize,
    // return pointer
    ret: Vec<usize>,
}

impl Vm {
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

    pub fn clear(&mut self) {
        self.frames.clear();
        self.ret.clear();
        self.fp = 0;
    }

    pub fn interpret(
        &mut self,
        instructions: Vec<Opcode>,
        state: &mut State,
    ) -> FerryResult<Value> {
        let program = Rc::new(RefCell::new(instructions));
        let mut frame = Frame {
            stack: vec![],
            pc: 0,
            function: program,
            locals: HashMap::new(),
        };

        for (key, value) in state.load_memory().drain() {
            frame.locals.insert(key, value.unwrap());
        }

        self.frames.push(frame);
        let result = self.run(state);
        if !self.frames[self.fp].stack.is_empty() {
            println!("STACK DID NOT CLEAR");
            println!("STACK: {:?}", self.frames[self.fp].stack);
            self.frames[self.fp].stack = vec![];
        }
        for (key, value) in self.frames[self.fp].locals.drain() {
            state.add_symbol(&key, Some(value));
        }

        result
    }

    fn run(&mut self, state: &mut State) -> FerryResult<Value> {
        loop {
            let instruction = self.advance(Rc::clone(&self.frames[self.fp].function));

            match instruction {
                Opcode::Nop => println!("nop"),
                Opcode::Halt => {
                    return Ok(Value::Unit);
                }
                Opcode::Return => {
                    let mut result = match self.frames[self.fp].stack.pop() {
                        Some(v) => v,
                        None => Value::Unit,
                    };
                    if let Value::Ptr(ptr) = result {
                        result = self.heap.get(&ptr).unwrap().clone();
                    }
                    if self.fp == 0 {
                        self.frames[self.fp].stack.clear();
                        return Ok(result);
                    } else {
                        self.frames[self.fp].stack.clear();
                        self.fp -= 1;

                        self.frames.pop();
                        self.frames[self.fp].stack.push(result);
                    }
                }
                // FerryOpcode::Load => self.frames[self.fp].pop(),
                Opcode::LoadI(c) => self.frames[self.fp].stack.push(c.into()),
                Opcode::Alloc(_ptr, a) => {
                    // self.heap.insert(ptr, a);
                    self.frames[self.fp].stack.push(a);
                }
                Opcode::Set(id) => {
                    let value = self.frames[self.fp].stack.pop().unwrap();
                    self.frames[self.fp].locals.insert(id, value);
                }
                Opcode::Get(id) => {
                    let value = self.frames[self.fp].locals.get(&id).unwrap().to_owned();
                    if let Value::Ptr(ptr) = value {
                        self.frames[self.fp]
                            .stack
                            .push(self.heap.get(&ptr).unwrap().clone());
                    } else {
                        self.frames[self.fp].stack.push(value);
                    }
                }
                Opcode::Pop => {
                    self.frames[self.fp].stack.pop().unwrap();
                }
                Opcode::Add => {
                    if self.frames[self.fp].stack.len() >= 2 {
                        let right: i64 = self.frames[self.fp]
                            .stack
                            .pop()
                            .unwrap()
                            .try_into()
                            .unwrap();
                        let left: i64 = self.frames[self.fp]
                            .stack
                            .pop()
                            .unwrap()
                            .try_into()
                            .unwrap();
                        let res: i64 = left + right;

                        self.frames[self.fp].stack.push(res.into());
                    } else {
                        return Err(FerryVmError::RuntimeError {
                            advice: "Invalid addition".into(),
                        });
                    }
                }
                Opcode::Sub => {
                    if self.frames[self.fp].stack.len() >= 2 {
                        let right: i64 = self.frames[self.fp]
                            .stack
                            .pop()
                            .unwrap()
                            .try_into()
                            .unwrap();
                        let left: i64 = self.frames[self.fp]
                            .stack
                            .pop()
                            .unwrap()
                            .try_into()
                            .unwrap();
                        let res: i64 = left - right;
                        self.frames[self.fp].stack.push(res.into());
                    } else {
                        return Err(FerryVmError::RuntimeError {
                            advice: "Invalid subtraction".into(),
                        });
                    }
                }
                Opcode::Mul => {
                    if self.frames[self.fp].stack.len() >= 2 {
                        let right: i64 = self.frames[self.fp]
                            .stack
                            .pop()
                            .unwrap()
                            .try_into()
                            .unwrap();
                        let left: i64 = self.frames[self.fp]
                            .stack
                            .pop()
                            .unwrap()
                            .try_into()
                            .unwrap();
                        let res: i64 = left * right;
                        self.frames[self.fp].stack.push(res.into());
                    } else {
                        return Err(FerryVmError::RuntimeError {
                            advice: "Invalid multiplication".into(),
                        });
                    }
                }
                Opcode::Div => {
                    if self.frames[self.fp].stack.len() >= 2 {
                        let right: i64 = self.frames[self.fp]
                            .stack
                            .pop()
                            .unwrap()
                            .try_into()
                            .unwrap();
                        let left: i64 = self.frames[self.fp]
                            .stack
                            .pop()
                            .unwrap()
                            .try_into()
                            .unwrap();
                        if right == 0 {
                            return Err(FerryVmError::RuntimeError {
                                advice: "DIVIDE BY ZERO".into(),
                            });
                        }
                        let res: i64 = left / right;
                        self.frames[self.fp].stack.push(res.into());
                    } else {
                        return Err(FerryVmError::RuntimeError {
                            advice: "Invalid division".into(),
                        });
                    }
                }
                Opcode::And => todo!(),
                Opcode::Or => todo!(),
                Opcode::Not => {
                    if let Value::Boolean(v) = self.frames[self.fp].stack.pop().unwrap() {
                        self.frames[self.fp].stack.push(Value::Boolean(!v));
                    } else {
                        self.frames[self.fp].stack.push(Value::Boolean(false));
                    }
                }
                Opcode::Equal => {
                    let right: i64 = self.frames[self.fp]
                        .stack
                        .pop()
                        .unwrap()
                        .try_into()
                        .unwrap();
                    let left: i64 = self.frames[self.fp]
                        .stack
                        .pop()
                        .unwrap()
                        .try_into()
                        .unwrap();
                    let res = left == right;
                    self.frames[self.fp].stack.push(res.into());
                }
                Opcode::Greater => {
                    let right: i64 = self.frames[self.fp]
                        .stack
                        .pop()
                        .unwrap()
                        .try_into()
                        .unwrap();
                    let left: i64 = self.frames[self.fp]
                        .stack
                        .pop()
                        .unwrap()
                        .try_into()
                        .unwrap();
                    let res = left > right;
                    self.frames[self.fp].stack.push(res.into());
                }
                Opcode::Lesser => {
                    let right: i64 = self.frames[self.fp]
                        .stack
                        .pop()
                        .unwrap()
                        .try_into()
                        .unwrap();
                    let left: i64 = self.frames[self.fp]
                        .stack
                        .pop()
                        .unwrap()
                        .try_into()
                        .unwrap();
                    let res = left < right;
                    self.frames[self.fp].stack.push(res.into());
                }
                Opcode::GetI => {
                    let right: i64 = self.frames[self.fp]
                        .stack
                        .pop()
                        .unwrap()
                        .try_into()
                        .unwrap();
                    let left: Vec<Value> = self.frames[self.fp]
                        .stack
                        .pop()
                        .unwrap()
                        .try_into()
                        .unwrap();
                    let res: Value = left[right as usize].clone();
                    self.frames[self.fp].stack.push(res);
                }
                Opcode::Cons => {
                    let right: Value = self.frames[self.fp].stack.pop().unwrap();
                    let mut left: Vec<Value> = self.frames[self.fp]
                        .stack
                        .pop()
                        .unwrap()
                        .try_into()
                        .unwrap();
                    let res: Vec<Value>;
                    if let Value::List(l) = right {
                        res = [left, l].concat();
                    } else {
                        left.push(right);
                        res = left;
                    }
                    self.frames[self.fp].stack.push(Value::List(res));
                }
                Opcode::Jump(offset) => {
                    self.frames[self.fp].pc += offset;
                }
                Opcode::JumpCond(offset) => {
                    let cond = self.frames[self.fp].stack.pop().unwrap();
                    if !cond.truthiness() {
                        self.frames[self.fp].pc += offset;
                    }
                }
                Opcode::JumpBack(offset) => {
                    self.frames[self.fp].pc -= offset;
                }
                Opcode::Iter => {
                    let iter: Vec<Value> = self.frames[self.fp]
                        .stack
                        .pop()
                        .unwrap()
                        .try_into()
                        .unwrap();

                    let (head, tail) = iter.split_first().unwrap();

                    let tail_len = tail.len() as i64;
                    self.frames[self.fp]
                        .stack
                        .push(Value::List(tail.into()));
                    // push len onto stack
                    self.frames[self.fp]
                        .stack
                        .push(Value::Number(tail_len));
                    // push value of variable assignment
                    self.frames[self.fp].stack.push(head.clone());
                }
                Opcode::Label(_label) => {
                    // this opcode may self destruct (deprecated?)
                    println!("lol");
                }
                Opcode::Call(label) => {
                    self.ret.push(self.frames[self.fp].pc);
                    if let Some(Value::Function(f)) = state.get_symbol_value(&label) {
                        let stack_len = self.frames[self.fp].stack.len();
                        let mut frame_stack = vec![];
                        for _ in (stack_len - f.arity)..stack_len {
                            frame_stack.push(self.frames[self.fp].stack.pop().unwrap());
                        }
                        let frame = Frame {
                            stack: frame_stack,
                            pc: 0,
                            function: Rc::new(RefCell::new(f.instructions)),
                            locals: HashMap::new(),
                        };

                        self.frames.push(frame);
                        self.fp += 1;
                    } else {
                        return Err(FerryVmError::RuntimeError {
                            advice: "Function call did not succeed".into(),
                        });
                    }
                }
                Opcode::JumpRet => {
                    self.frames[self.fp].pc = self.ret.pop().unwrap();
                }
            }
        }
    }

    fn advance(&mut self, instructions: Rc<RefCell<Vec<Opcode>>>) -> Opcode {
        self.frames[self.fp].pc += 1;
        instructions.borrow_mut()[self.frames[self.fp].pc - 1].to_owned()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn check_run() {
        let mut vm = Vm::new();
        let instructions = vec![Opcode::Halt];
        assert!(vm.interpret(instructions, &mut State::new()).unwrap() == Value::Unit);
    }

    #[test]
    #[ignore]
    fn check_load() {
        let mut vm = Vm::new();
        let instructions = vec![
            Opcode::LoadI(1),
            Opcode::LoadI(2),
            Opcode::Halt,
        ];
        vm.interpret(instructions, &mut State::new()).unwrap();
        assert!(vm.frames[vm.fp].stack == vec![Value::Number(1), Value::Number(2)]);
    }

    #[test]
    fn check_add() {
        let mut vm = Vm::new();
        let instructions = vec![
            Opcode::LoadI(1),
            Opcode::LoadI(2),
            Opcode::Add,
            Opcode::Return,
            Opcode::Halt,
        ];
        assert!(
            vm.interpret(instructions, &mut State::new()).unwrap() == Value::Number(3)
        );
    }
}
