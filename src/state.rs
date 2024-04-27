use std::collections::HashMap;

use crate::types::{FerryType, FerryTyping, TypeCheckable};

// placeholder for program state
#[derive(Debug, Clone, PartialEq)]
pub struct FerryState {
    symbols: HashMap<String, Option<FerryValue>>,
}

#[derive(Debug, Clone, PartialEq)]
pub enum FerryValue {
    Number(f64),
    Str(String),
    Boolean(bool),
    Unit,
}

impl FerryState {
    pub fn new() -> Self {
        Self {
            symbols: HashMap::new(),
        }
    }

    pub fn add_symbol(&mut self, id: &String, value: Option<FerryValue>) {
        if !self.symbols.contains_key(id) {
            self.symbols.insert(id.clone(), value);
        } else {
            self.update_symbol(&id, value);
        }
    }

    fn update_symbol(&mut self, id: &String, value: Option<FerryValue>) {
        if self.symbols.contains_key(id) {
            *self.symbols.get_mut(id).unwrap() = value.clone();
        }
    }

    pub fn get_symbol_value(&self, id: &String) -> Option<FerryValue> {
        if self.symbols.contains_key(id) {
            self.symbols.get(id).unwrap().clone()
        } else {
            None
        }
    }
}

impl std::fmt::Display for FerryState {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        for k in self.symbols.keys() {
            if let Some(v) = self.symbols.get(k).unwrap() {
                write!(f, "{}: {}\n", k, v)?;
            } else {
                write!(f, "{}: undefined\n", k)?;
            }
        }

        Ok(())
    }
}

impl FerryValue {
    pub fn verify_type(&self, t: &FerryType) -> bool {
        self.get_type() == t
    }
}

impl TypeCheckable for FerryValue {
    fn get_type(&self) -> &FerryType {
        match self {
            FerryValue::Number(_) => &FerryType::Num,
            FerryValue::Str(_) => &FerryType::String,
            FerryValue::Boolean(_) => &FerryType::Boolean,
            FerryValue::Unit => &FerryType::Undefined,
        }
    }

    fn check(&self, other: &FerryTyping) -> bool {
        self.get_type() == other.get_type()
    }
}

impl std::fmt::Display for FerryValue {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            FerryValue::Number(n) => write!(f, "{}", n),
            FerryValue::Str(s) => write!(f, "{s}"),
            FerryValue::Boolean(b) => write!(f, "{b}"),
            FerryValue::Unit => write!(f, "[unit]"),
        }
    }
}
