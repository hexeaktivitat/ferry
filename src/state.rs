use std::collections::HashMap;

use crate::{
    ir::FerryOpcode,
    syntax::Function,
    types::{FerryType, TypeCheckable, Typing},
};

// placeholder for program state
#[derive(Debug, Clone)]
pub struct FerryState {
    symbols: HashMap<String, Option<FerryValue>>,
    labels: HashMap<String, usize>,
}

#[derive(Debug, Clone, PartialEq)]
pub enum FerryValue {
    Number(i64),
    Str(String),
    Boolean(bool),
    List(Vec<FerryValue>),
    Function {
        declaration: Option<Function>,
        name: String,
        func_type: FerryType,
        instructions: Vec<FerryOpcode>,
        arity: usize,
    },
    Ptr(u8),
    Unit,
}

// i'll make my OWN From<T>! with BLACKJACK! and HOOKERS!
pub trait Convertable<T>
where
    Self: Sized,
{
    fn convert_from(value: T) -> Self;
    fn convert_to(self) -> T;
}

impl Convertable<i64> for FerryValue {
    fn convert_from(value: i64) -> Self {
        FerryValue::Number(value)
    }

    fn convert_to(self) -> i64 {
        if let FerryValue::Number(value) = self {
            value
        } else {
            // coerce non-Number values to 0
            0
        }
    }
}

impl Convertable<String> for FerryValue {
    fn convert_from(value: String) -> Self {
        FerryValue::Str(value)
    }

    fn convert_to(self) -> String {
        if let FerryValue::Str(value) = self {
            value
        } else {
            "".into()
        }
    }
}

impl Convertable<Vec<FerryValue>> for FerryValue {
    fn convert_from(value: Vec<FerryValue>) -> Self {
        FerryValue::List(value)
    }

    fn convert_to(self) -> Vec<FerryValue> {
        if let FerryValue::List(value) = self {
            value
        } else {
            vec![]
        }
    }
}

impl Convertable<bool> for FerryValue {
    fn convert_from(value: bool) -> Self {
        FerryValue::Boolean(value)
    }

    fn convert_to(self) -> bool {
        if let FerryValue::Boolean(value) = self {
            value
        } else {
            false
        }
    }
}

impl FerryState {
    pub fn new() -> Self {
        Self {
            symbols: HashMap::new(),
            labels: HashMap::new(),
        }
    }

    pub fn add_symbol(&mut self, id: &String, value: Option<FerryValue>) {
        if !self.symbols.contains_key(id) {
            self.symbols.insert(id.clone(), value);
        } else {
            self.update_symbol(id, value);
        }
    }

    fn update_symbol(&mut self, id: &String, value: Option<FerryValue>) {
        if self.symbols.contains_key(id) {
            self.symbols.get_mut(id).unwrap().clone_from(&value);
        }
    }

    pub fn get_symbol_value(&self, id: &String) -> Option<FerryValue> {
        if self.symbols.contains_key(id) {
            self.symbols.get(id).unwrap().clone()
        } else {
            None
        }
    }

    pub fn add_label(&mut self, id: &str, value: usize) {
        self.labels.insert(id.into(), value);
    }

    pub fn _get_label(&self, id: &String) -> Option<usize> {
        if self.labels.contains_key(id) {
            Some(*self.labels.get(id).unwrap())
        } else {
            None
        }
    }
}

impl std::fmt::Display for FerryState {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        for k in self.symbols.keys() {
            if let Some(v) = self.symbols.get(k).unwrap() {
                writeln!(f, "{}: {}", k, v)?;
            } else {
                writeln!(f, "{}: undefined", k)?;
            }
        }

        Ok(())
    }
}

impl Typing for FerryValue {
    fn get_type(&self) -> &FerryType {
        match self {
            FerryValue::Number(_) => &FerryType::Num,
            FerryValue::Str(_) => &FerryType::String,
            FerryValue::Boolean(_) => &FerryType::Boolean,
            FerryValue::Unit => &FerryType::Undefined,
            FerryValue::List(_) => &FerryType::List,
            FerryValue::Function {
                declaration: _,
                name: _,
                func_type: expr_type,
                instructions: _,
                arity: _,
            } => expr_type,
            FerryValue::Ptr(_) => &FerryType::Pointer,
        }
    }
}

impl TypeCheckable for FerryValue {
    fn check(&self, other: &FerryType) -> bool {
        self.get_type() == other
    }
}

impl std::fmt::Display for FerryValue {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            FerryValue::Number(n) => write!(f, "{}", n),
            FerryValue::Str(s) => write!(f, "\"{s}\""),
            FerryValue::Boolean(b) => write!(f, "{b}"),
            FerryValue::Unit => write!(f, "[unit]"),
            FerryValue::List(l) => {
                let mut formatting = String::new();
                formatting.push('[');
                let mut items = l.iter().peekable();
                while let Some(item) = items.next() {
                    formatting.push_str(format!("{item}").as_str());
                    if items.peek().is_some() {
                        formatting.push_str(", ");
                    }
                }
                formatting.push(']');
                write!(f, "{formatting}")
            }
            FerryValue::Function {
                declaration: _,
                name: _,
                func_type: _,
                instructions: _,
                arity: _,
            } => write!(f, "function placeholder"),
            FerryValue::Ptr(p) => write!(f, "address: {p}"),
        }
    }
}

impl FerryValue {
    pub fn truthiness(&self) -> bool {
        match self {
            FerryValue::Number(n) => n > &0,
            FerryValue::Str(s) => !s.is_empty(),
            FerryValue::Boolean(b) => *b,
            FerryValue::Unit => false,
            FerryValue::List(l) => !l.is_empty(),
            FerryValue::Function {
                declaration: _,
                name: _,
                func_type: _,
                instructions: _,
                arity: _,
            } => false,
            FerryValue::Ptr(p) => !*p == 0xff,
        }
    }
}
