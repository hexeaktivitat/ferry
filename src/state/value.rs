// use anyhow::{anyhow, Error};

use crate::{ir::Opcode, parser::syntax::Function};

use super::types::FerryType;

// this was needed to impl From for FerryValue exhaustively
#[derive(Debug, Clone, PartialEq)]
pub struct FuncVal {
    pub declaration: Option<Function>,
    pub name: String,
    pub func_type: FerryType,
    pub instructions: Vec<Opcode>,
    pub arity: usize,
}

#[derive(Debug, Clone, PartialEq, Default)]
pub enum Value {
    Integer(i64),
    Str(String),
    Boolean(bool),
    List(Vec<Value>),
    Function(FuncVal),
    Ptr(u8),
    #[default]
    Unit,
}

impl From<i64> for Value {
    fn from(value: i64) -> Self {
        Value::Integer(value)
    }
}

// Coerces all non-Number values to 0
impl From<Value> for i64 {
    fn from(value: Value) -> Self {
        if let Value::Integer(v) = value { v } else { 0 }
    }
}

// impl TryFrom<Value> for i64 {
//     type Error = Error;

//     fn try_from(value: Value) -> Result<Self, Self::Error> {
//         if let Value::Number(v) = value {
//             Ok(v)
//         } else {
//             Err(anyhow!("not an i64"))
//         }
//     }
// }

impl From<String> for Value {
    fn from(value: String) -> Self {
        Value::Str(value)
    }
}

// coerces all non-Str to be empty Strings
impl From<Value> for String {
    fn from(value: Value) -> Self {
        if let Value::Str(v) = value {
            v
        } else {
            String::new()
        }
    }
}

// impl TryFrom<Value> for String {
//     type Error = Error;

//     fn try_from(value: Value) -> Result<Self, Self::Error> {
//         if let Value::Str(v) = value {
//             Ok(v)
//         } else {
//             Err(anyhow!("not a String"))
//         }
//     }
// }

impl From<Vec<Value>> for Value {
    fn from(value: Vec<Value>) -> Value {
        Value::List(value)
    }
}

// coerces empty Vec<Value> for List
impl From<Value> for Vec<Value> {
    fn from(value: Value) -> Self {
        if let Value::List(v) = value {
            v
        } else {
            Vec::new()
        }
    }
}

// impl TryFrom<Value> for Vec<Value> {
//     type Error = Error;

//     fn try_from(value: Value) -> Result<Self, Self::Error> {
//         if let Value::List(v) = value {
//             Ok(v)
//         } else {
//             Err(anyhow!("not a Vec<FerryValue>"))
//         }
//     }
// }

impl From<bool> for Value {
    fn from(value: bool) -> Self {
        Value::Boolean(value)
    }
}

impl From<Value> for bool {
    fn from(value: Value) -> Self {
        if let Value::Boolean(v) = value {
            v
        } else {
            false
        }
    }
}

// impl TryFrom<Value> for bool {
//     type Error = Error;

//     fn try_from(value: Value) -> Result<Self, Self::Error> {
//         if let Value::Boolean(v) = value {
//             Ok(v)
//         } else {
//             Err(anyhow!("Not a bool"))
//         }
//     }
// }

impl From<FuncVal> for Value {
    fn from(value: FuncVal) -> Self {
        Value::Function(value)
    }
}

impl From<Value> for FuncVal {
    fn from(value: Value) -> Self {
        if let Value::Function(v) = value {
            v
        } else {
            Self {
                declaration: None,
                name: String::new(),
                func_type: FerryType::Undefined,
                instructions: vec![Opcode::Nop],
                arity: 0,
            }
        }
    }
}

// impl TryFrom<Value> for FuncVal {
//     type Error = Error;

//     fn try_from(value: Value) -> Result<Self, Self::Error> {
//         if let Value::Function(v) = value {
//             Ok(v)
//         } else {
//             Err(anyhow!("Not a FuncVal"))
//         }
//     }
// }

impl From<u8> for Value {
    fn from(value: u8) -> Self {
        Value::Ptr(value)
    }
}

// null pointers????? null pointers....
impl From<Value> for u8 {
    fn from(value: Value) -> Self {
        if let Value::Ptr(v) = value { v } else { 0 }
    }
}

// impl TryFrom<Value> for u8 {
//     type Error = Error;

//     fn try_from(value: Value) -> Result<Self, Self::Error> {
//         if let Value::Ptr(v) = value {
//             Ok(v)
//         } else {
//             Err(anyhow!("Not a Ptr"))
//         }
//     }
// }

impl From<()> for Value {
    fn from(_value: ()) -> Self {
        Value::Unit
    }
}

impl From<Value> for () {
    fn from(_value: Value) -> Self {}
}

// impl TryFrom<Value> for () {
//     type Error = Error;

//     fn try_from(value: Value) -> Result<Self, Self::Error> {
//         if Value::Unit == value {
//             Ok(())
//         } else {
//             Err(anyhow!("not ()"))
//         }
//     }
// }
