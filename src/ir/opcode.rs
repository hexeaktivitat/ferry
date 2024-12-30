use crate::state::value::Value;

use super::FerryAddr;

#[derive(Debug, Clone, PartialEq)]
pub enum Opcode {
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
    Alloc(FerryAddr, Value),
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
impl Into<u8> for Opcode {
    fn into(self) -> u8 {
        match self {
            Opcode::Nop => 0x00,
            // FerryOpcode::Load => 0x01,
            Opcode::LoadI(_) => 0x02,
            Opcode::Alloc(_, _) => 0x03,
            Opcode::Set(_) => 0x04,
            Opcode::Get(_) => 0x05,
            Opcode::Pop => 0x06,
            Opcode::Add => 0x10,
            Opcode::Sub => 0x11,
            Opcode::Mul => 0x12,
            Opcode::Div => 0x13,
            Opcode::And => 0x14,
            Opcode::Or => 0x15,
            Opcode::Not => 0x16,
            Opcode::Equal => 0x17,
            Opcode::Greater => 0x18,
            Opcode::Lesser => 0x19,
            Opcode::GetI => 0x40,
            Opcode::Cons => 0x41,
            Opcode::Jump(_) => 0x20,
            Opcode::JumpCond(_) => 0x21,
            Opcode::JumpBack(_) => 0x22,
            Opcode::Iter => 0x23,
            Opcode::Label(_) => 0x24,
            Opcode::Call(_) => 0x25,
            Opcode::JumpRet => 0x26,
            Opcode::Return => 0xfe,
            Opcode::Halt => 0xff,
        }
    }
}

impl std::fmt::Display for Opcode {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Opcode::Nop => write!(f, "nop"),
            Opcode::Halt => write!(f, "halt"),
            Opcode::Return => write!(f, "return"),
            Opcode::LoadI(i) => write!(f, "loadi {i}"),
            Opcode::Alloc(p, ferry_value) => write!(f, "alloc @{p} {ferry_value}"),
            Opcode::Set(v) => write!(f, "set {v}"),
            Opcode::Get(v) => write!(f, "get {v}"),
            Opcode::Pop => write!(f, "pop"),
            Opcode::Add => write!(f, "add"),
            Opcode::Sub => write!(f, "sub"),
            Opcode::Mul => write!(f, "mul"),
            Opcode::Div => write!(f, "div"),
            Opcode::And => write!(f, "and"),
            Opcode::Or => write!(f, "or"),
            Opcode::Not => write!(f, "not"),
            Opcode::Equal => write!(f, "eq"),
            Opcode::Greater => write!(f, "greater"),
            Opcode::Lesser => write!(f, "lesser"),
            Opcode::GetI => write!(f, "geti"),
            Opcode::Cons => write!(f, "cons"),
            Opcode::Jump(d) => write!(f, "jump {d}"),
            Opcode::JumpCond(d) => write!(f, "jumpc {d}"),
            Opcode::JumpBack(d) => write!(f, "jumpb {d}"),
            Opcode::Label(l) => write!(f, "label #{l}"),
            Opcode::Call(func) => write!(f, "call #{func}"),
            Opcode::JumpRet => write!(f, "jumpr"),
            Opcode::Iter => write!(f, "iter"),
        }
    }
}
