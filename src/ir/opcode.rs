use crate::state::value::FerryValue;

use super::FerryAddr;

#[derive(Debug, Clone, PartialEq)]
pub enum FerryOpcode {
    // NOP: no operation
    Nop,
    // HALT: terminate application
    Halt,
    Return,
    // LOAD: push a FerryValue onto stack
    // Load,
    // LOADI: loads designated value (push onto stack)
    LoadI(i64),
    // ALLOC: allocates on the heap vs stack
    Alloc(FerryAddr, FerryValue),
    Set(String),
    Get(String),
    // POP: pops and discards top stack value
    Pop,

    // ADD: pops last 2 values, adds, pushes onto stack
    Add,
    Sub,
    Mul,
    Div,
    And,
    Or,
    Not,
    Equal,
    Greater,
    Lesser,
    GetI,
    Cons,
    // JUMP: specifies the offset for a jump operation
    Jump(usize),
    // JUMPCOND: only jumps if top stack value is truthy
    JumpCond(usize),
    JumpBack(usize),
    Label(String),
    Call(String),
    JumpRet,
    Iter,
}

// Into over From due to not being able to effeciently map u8 to fixed enum values
#[expect(clippy::from_over_into)]
impl Into<u8> for FerryOpcode {
    fn into(self) -> u8 {
        match self {
            FerryOpcode::Nop => 0x00,
            // FerryOpcode::Load => 0x01,
            FerryOpcode::LoadI(_) => 0x02,
            FerryOpcode::Alloc(_, _) => 0x03,
            FerryOpcode::Set(_) => 0x04,
            FerryOpcode::Get(_) => 0x05,
            FerryOpcode::Pop => 0x06,
            FerryOpcode::Add => 0x10,
            FerryOpcode::Sub => 0x11,
            FerryOpcode::Mul => 0x12,
            FerryOpcode::Div => 0x13,
            FerryOpcode::And => 0x14,
            FerryOpcode::Or => 0x15,
            FerryOpcode::Not => 0x16,
            FerryOpcode::Equal => 0x17,
            FerryOpcode::Greater => 0x18,
            FerryOpcode::Lesser => 0x19,
            FerryOpcode::GetI => 0x40,
            FerryOpcode::Cons => 0x41,
            FerryOpcode::Jump(_) => 0x20,
            FerryOpcode::JumpCond(_) => 0x21,
            FerryOpcode::JumpBack(_) => 0x22,
            FerryOpcode::Iter => 0x23,
            FerryOpcode::Label(_) => 0x24,
            FerryOpcode::Call(_) => 0x25,
            FerryOpcode::JumpRet => 0x26,
            FerryOpcode::Return => 0xfe,
            FerryOpcode::Halt => 0xff,
        }
    }
}

impl std::fmt::Display for FerryOpcode {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            FerryOpcode::Nop => write!(f, "nop"),
            FerryOpcode::Halt => write!(f, "halt"),
            FerryOpcode::Return => write!(f, "return"),
            FerryOpcode::LoadI(i) => write!(f, "loadi {i}"),
            FerryOpcode::Alloc(p, ferry_value) => write!(f, "alloc @{p} {ferry_value}"),
            FerryOpcode::Set(v) => write!(f, "set {v}"),
            FerryOpcode::Get(v) => write!(f, "get {v}"),
            FerryOpcode::Pop => write!(f, "pop"),
            FerryOpcode::Add => write!(f, "add"),
            FerryOpcode::Sub => write!(f, "sub"),
            FerryOpcode::Mul => write!(f, "mul"),
            FerryOpcode::Div => write!(f, "div"),
            FerryOpcode::And => write!(f, "and"),
            FerryOpcode::Or => write!(f, "or"),
            FerryOpcode::Not => write!(f, "not"),
            FerryOpcode::Equal => write!(f, "eq"),
            FerryOpcode::Greater => write!(f, "greater"),
            FerryOpcode::Lesser => write!(f, "lesser"),
            FerryOpcode::GetI => write!(f, "geti"),
            FerryOpcode::Cons => write!(f, "cons"),
            FerryOpcode::Jump(d) => write!(f, "jump {d}"),
            FerryOpcode::JumpCond(d) => write!(f, "jumpc {d}"),
            FerryOpcode::JumpBack(d) => write!(f, "jumpb {d}"),
            FerryOpcode::Label(l) => write!(f, "label #{l}"),
            FerryOpcode::Call(func) => write!(f, "call #{func}"),
            FerryOpcode::JumpRet => write!(f, "jumpr"),
            FerryOpcode::Iter => write!(f, "iter"),
        }
    }
}
