use std::{collections::HashMap, rc::Rc};

#[derive(Clone, Debug, PartialEq)]
pub enum SymbolScope {
    Global,
    Local,
    Builtin,
    Free,
    Func,
}

#[derive(Clone, Debug, PartialEq)]
pub struct Symbol {
    pub name: String,
    pub scope: SymbolScope,
    pub idx: usize,
}

#[derive(Clone, Debug, PartialEq, Default)]
pub struct SymbolTable {
    pub store: HashMap<String, Rc<Symbol>>,
    pub outer: Option<Rc<SymbolTable>>,
    pub free: Vec<Rc<Symbol>>,
    pub n_defs: usize,
}

impl SymbolTable {
    pub fn new() -> Self {
        SymbolTable {
            store: HashMap::new(),
            outer: None,
            free: vec![],
            n_defs: 0,
        }
    }

    pub fn new_enclosed(outer: &SymbolTable) -> Self {
        SymbolTable {
            store: HashMap::new(),
            outer: Some(Rc::new(outer.clone())),
            free: vec![],
            n_defs: 0,
        }
    }

    pub fn get(&self, key: &str) -> Option<Rc<Symbol>> {
        if let Some(s) = self.store.get(key) {
            Some(Rc::clone(s))
        } else {
            self.outer.as_ref().and_then(|o| o.get(key))
        }
    }

    pub fn set(&mut self, key: String) -> Rc<Symbol> {
        let symbol = Rc::new(Symbol {
            name: key.clone(),
            scope: match self.outer {
                Some(_) => SymbolScope::Local,
                None => SymbolScope::Global,
            },
            idx: self.n_defs,
        });

        self.n_defs += 1;
        self.store.insert(key, Rc::clone(&symbol));

        symbol
    }
}
