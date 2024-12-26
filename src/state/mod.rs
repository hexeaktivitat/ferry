use std::collections::HashMap;

use types::{FerryType, TypeCheckable, Typing};
use value::FerryValue;

pub(crate) mod types;
pub(crate) mod value;

// placeholder for program state
#[derive(Debug, Clone)]
pub struct FerryState {
    variables: HashMap<String, Option<FerryValue>>,
    labels: HashMap<String, usize>,
}

impl FerryState {
    pub fn new() -> Self {
        Self {
            variables: HashMap::new(),
            labels: HashMap::new(),
        }
    }

    pub fn add_symbol(&mut self, id: &String, value: Option<FerryValue>) {
        if !self.variables.contains_key(id) {
            self.variables.insert(id.clone(), value);
        } else {
            self.update_symbol(id, value);
        }
    }

    fn update_symbol(&mut self, id: &String, value: Option<FerryValue>) {
        if self.variables.contains_key(id) {
            self.variables.get_mut(id).unwrap().clone_from(&value);
        }
    }

    pub fn get_symbol_value(&self, id: &String) -> Option<FerryValue> {
        if self.variables.contains_key(id) {
            self.variables.get(id).unwrap().clone()
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
        for k in self.variables.keys() {
            if let Some(v) = self.variables.get(k).unwrap() {
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
            FerryValue::Function(f) => &f.func_type,
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
            FerryValue::Function(function) => {
                let mut formatting = String::new();
                formatting.push('(');
                if let Some(func) = &function.declaration {
                    if let Some(args) = &func.args {
                        let mut args_iter = args.iter().peekable();
                        while let Some(arg) = args_iter.next() {
                            formatting.push_str(format!("{}", arg.get_type()).as_str());
                            if args_iter.peek().is_some() {
                                formatting.push_str(", ");
                            }
                        }
                    }
                    formatting.push(')');
                    if let Some(return_type) = &func.return_type {
                        formatting.push_str(format!(" -> {}", return_type).as_str());
                    } else {
                        formatting.push_str("[unit]");
                    }
                }

                write!(f, "{formatting}")
            }
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
            FerryValue::Function(_) => false,
            FerryValue::Ptr(p) => !*p == 0xff,
        }
    }
}
