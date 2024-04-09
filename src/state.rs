use std::collections::HashMap;

// placeholder for program state
#[derive(Debug, Clone, PartialEq)]
pub struct FerryState {
    symbols: HashMap<String, Option<FerryValue>>,
}

#[derive(Debug, Clone, PartialEq)]
pub enum FerryValue {
    Number(f64),
}

impl FerryState {
    pub fn new() -> Self {
        Self {
            symbols: HashMap::new(),
        }
    }

    pub fn add_symbol(&mut self, id: String, value: Option<FerryValue>) {
        if !self.symbols.contains_key(&id) {
            self.symbols.insert(id, value);
        }
    }

    pub fn get_symbol_value(&self, id: &String) -> Option<FerryValue> {
        self.symbols.get(id).unwrap().clone()
    }
}
