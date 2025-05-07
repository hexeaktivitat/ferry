use std::collections::HashMap;

use miette::{Error, miette};
use symbol::Symbol;
use types::{FerryType, Typing};
use value::Value;

pub(crate) mod symbol;
pub(crate) mod types;
pub(crate) mod value;

// placeholder for program state
#[derive(Debug, Clone)]
pub struct State {
    var_values: HashMap<String, Option<Value>>,
    labels: HashMap<String, usize>,
    var_symbols: HashMap<String, Symbol>,
}

impl State {
    pub fn new() -> Self {
        Self {
            var_values: HashMap::new(),
            var_symbols: HashMap::new(),
            labels: HashMap::new(),
        }
    }

    pub fn add_variable(&mut self, id: &str, value: Option<Value>) {
        if self.var_values.contains_key(id) {
            self.update_variable(id, value);
        } else {
            self.var_values.insert(id.into(), value);
        }
    }

    fn update_variable(&mut self, id: &str, value: Option<Value>) {
        if self.var_values.contains_key(id) {
            self.var_values.get_mut(id).unwrap().clone_from(&value);
        }
    }

    pub fn get_variable_value(&self, id: &str) -> Option<Value> {
        if self.var_values.contains_key(id) {
            self.var_values.get(id).unwrap().clone()
        } else {
            None
        }
    }

    pub fn add_label(&mut self, id: &str, value: usize) {
        self.labels.insert(id.into(), value);
    }

    // currently simply clones the variables instance for consumption via drain()
    pub fn load_memory(&self) -> HashMap<String, Option<Value>> {
        self.var_values.clone()
    }

    pub fn _get_label(&self, id: &str) -> Option<usize> {
        if self.labels.contains_key(id) {
            Some(*self.labels.get(id).unwrap())
        } else {
            None
        }
    }

    pub fn get_symbol_table(&self) -> &HashMap<String, Symbol> {
        &self.var_symbols
    }

    pub fn add_symbol(&mut self, symbol: Symbol) -> Result<(), Error> {
        if self.var_symbols.contains_key(&symbol.identifier) {
            // Err(miette!("ident already tied to a symbol in scope"))
            self.add_use_count(&symbol.identifier)
        } else {
            self.var_symbols.insert(symbol.identifier.clone(), symbol);
            Ok(())
        }
    }

    pub fn get_symbol(&mut self, id: &str) -> Result<&mut Symbol, Error> {
        self.var_symbols
            .get_mut(id)
            .ok_or_else(|| miette!("variable symbol does not exist"))

        // .ok_or(Err(miette!("variable symbol does not exist")))
    }

    pub fn initialize_symbol(&mut self, id: &str) -> Result<(), Error> {
        let Some(symbol) = self.var_symbols.get_mut(id) else {
            return Err(miette!("variable symbol does not exist for initialization"));
        };

        symbol.initialized = true;
        Ok(())
    }

    pub fn declare_symbol(&mut self, id: &str) -> Result<(), Error> {
        let Some(symbol) = self.var_symbols.get_mut(id) else {
            return Err(miette!("variable symbol does not exist for declaration"));
        };

        symbol.declare()
    }

    pub fn add_use_count(&mut self, id: &str) -> Result<(), Error> {
        let Some(symbol) = self.var_symbols.get_mut(id) else {
            return Err(miette!("variable symbol does not exist for initialization"));
        };

        symbol.use_count += 1;
        Ok(())
    }
}

impl std::fmt::Display for State {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        for k in self.var_values.keys() {
            if let Some(v) = self.var_values.get(k).unwrap() {
                writeln!(f, "{k}: {v}")?;
            } else {
                writeln!(f, "{k}: undefined")?;
            }
        }

        Ok(())
    }
}

impl Typing for Value {
    fn get_type(&self) -> &FerryType {
        match self {
            Value::Integer(_) => &FerryType::Int,
            Value::Str(_) => &FerryType::String,
            Value::Boolean(_) => &FerryType::Boolean,
            Value::Unit => &FerryType::Undefined,
            Value::List(_) => &FerryType::List,
            Value::Function(f) => &f.func_type,
            Value::Ptr(_) => &FerryType::Pointer,
        }
    }

    fn check(&self, other: &FerryType) -> bool {
        self.get_type() == other
    }
}

impl std::fmt::Display for Value {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Value::Integer(n) => write!(f, "{}", n),
            Value::Str(s) => write!(f, "\"{s}\""),
            Value::Boolean(b) => write!(f, "{b}"),
            Value::Unit => write!(f, "[unit]"),
            Value::List(l) => {
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
            Value::Function(function) => {
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
                        formatting.push_str(format!(" -> {return_type}").as_str());
                    } else {
                        formatting.push_str("[unit]");
                    }
                }

                write!(f, "{formatting}")
            }
            Value::Ptr(p) => write!(f, "address: {p}"),
        }
    }
}

impl Value {
    pub fn truthiness(&self) -> bool {
        match self {
            Value::Integer(n) => n > &0,
            Value::Str(s) => !s.is_empty(),
            Value::Boolean(b) => *b,
            Value::List(l) => !l.is_empty(),
            Value::Function(_) | Value::Unit => false,
            Value::Ptr(p) => !*p == 0xff,
        }
    }
}
