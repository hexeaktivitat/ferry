use anyhow::{anyhow, Error};

use crate::{ir::FerryOpcode, parser::syntax::Function};

use super::types::FerryType;

// this was needed to impl From for FerryValue exhaustively
#[derive(Debug, Clone, PartialEq)]
pub struct FuncVal {
    pub declaration: Option<Function>,
    pub name: String,
    pub func_type: FerryType,
    pub instructions: Vec<FerryOpcode>,
    pub arity: usize,
}

#[derive(Debug, Clone, PartialEq)]
pub enum FerryValue {
    Number(i64),
    Str(String),
    Boolean(bool),
    List(Vec<FerryValue>),
    Function(FuncVal),
    Ptr(u8),
    Unit,
}

impl From<i64> for FerryValue {
    fn from(value: i64) -> Self {
        FerryValue::Number(value)
    }
}

impl TryFrom<FerryValue> for i64 {
    type Error = Error;

    fn try_from(value: FerryValue) -> Result<Self, Self::Error> {
        if let FerryValue::Number(v) = value {
            Ok(v)
        } else {
            Err(anyhow!("not an i64"))
        }
    }
}

impl From<String> for FerryValue {
    fn from(value: String) -> Self {
        FerryValue::Str(value)
    }
}

impl TryFrom<FerryValue> for String {
    type Error = Error;

    fn try_from(value: FerryValue) -> Result<Self, Self::Error> {
        if let FerryValue::Str(v) = value {
            Ok(v)
        } else {
            Err(anyhow!("not a String"))
        }
    }
}

impl From<Vec<FerryValue>> for FerryValue {
    fn from(value: Vec<FerryValue>) -> FerryValue {
        FerryValue::List(value)
    }
}

impl TryFrom<FerryValue> for Vec<FerryValue> {
    type Error = Error;

    fn try_from(value: FerryValue) -> Result<Self, Self::Error> {
        if let FerryValue::List(v) = value {
            Ok(v)
        } else {
            Err(anyhow!("not a Vec<FerryValue>"))
        }
    }
}

impl From<bool> for FerryValue {
    fn from(value: bool) -> Self {
        FerryValue::Boolean(value)
    }
}

impl TryFrom<FerryValue> for bool {
    type Error = Error;

    fn try_from(value: FerryValue) -> Result<Self, Self::Error> {
        if let FerryValue::Boolean(v) = value {
            Ok(v)
        } else {
            Err(anyhow!("Not a bool"))
        }
    }
}

impl From<FuncVal> for FerryValue {
    fn from(value: FuncVal) -> Self {
        FerryValue::Function(value)
    }
}

impl TryFrom<FerryValue> for FuncVal {
    type Error = Error;

    fn try_from(value: FerryValue) -> Result<Self, Self::Error> {
        if let FerryValue::Function(v) = value {
            Ok(v)
        } else {
            Err(anyhow!("Not a FuncVal"))
        }
    }
}

impl From<u8> for FerryValue {
    fn from(value: u8) -> Self {
        FerryValue::Ptr(value)
    }
}

impl TryFrom<FerryValue> for u8 {
    type Error = Error;

    fn try_from(value: FerryValue) -> Result<Self, Self::Error> {
        if let FerryValue::Ptr(v) = value {
            Ok(v)
        } else {
            Err(anyhow!("Not a Ptr"))
        }
    }
}

impl From<()> for FerryValue {
    fn from(_value: ()) -> Self {
        FerryValue::Unit
    }
}

impl TryFrom<FerryValue> for () {
    type Error = Error;

    fn try_from(value: FerryValue) -> Result<Self, Self::Error> {
        if FerryValue::Unit == value {
            Ok(())
        } else {
            Err(anyhow!("not ()"))
        }
    }
}
