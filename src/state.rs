use std::collections::HashMap;

use crate::{
    syntax::Function,
    types::{FerryType, TypeCheckable, Typing},
};

// placeholder for program state
#[derive(Debug, Clone, PartialEq)]
pub struct FerryState {
    symbols: HashMap<String, Option<FerryValue>>,
}

#[derive(Debug, Clone, PartialEq)]
pub enum FerryValue {
    Number(i64),
    Str(String),
    Boolean(bool),
    List(Vec<FerryValue>),
    Function { declaration: Function },
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
            FerryValue::Function { declaration } => declaration.contents.get_type(),
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
            FerryValue::Function { declaration: _ } => write!(f, "function placeholder"),
        }
    }
}
