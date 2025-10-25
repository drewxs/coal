use std::{cell::RefCell, collections::HashMap, rc::Rc};

use coal_objects::builtin_defs;

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

#[derive(Clone, Debug, PartialEq)]
pub struct SymbolTable {
    pub store: Rc<RefCell<HashMap<String, Rc<Symbol>>>>,
    pub outer: Option<Rc<SymbolTable>>,
    pub free: Rc<RefCell<Vec<Rc<Symbol>>>>,
    pub n_defs: usize,
}

impl SymbolTable {
    pub fn new() -> Self {
        let mut st = SymbolTable {
            store: Rc::new(RefCell::new(HashMap::new())),
            outer: None,
            free: Rc::new(RefCell::new(vec![])),
            n_defs: 0,
        };

        for (i, b) in builtin_defs().iter().enumerate() {
            st.define_builtin(i, b.name.to_owned());
        }

        st
    }

    pub fn new_enclosed(outer: &SymbolTable) -> Self {
        SymbolTable {
            store: Rc::new(RefCell::new(HashMap::new())),
            outer: Some(Rc::new(outer.clone())),
            free: Rc::new(RefCell::new(vec![])),
            n_defs: 0,
        }
    }

    pub fn get(&self, key: &str) -> Option<Rc<Symbol>> {
        if let Some(sym) = self.store.borrow().get(key) {
            return Some(Rc::clone(sym));
        }

        if let Some(outer) = &self.outer
            && let Some(s) = outer.get(key)
        {
            if matches!(s.scope, SymbolScope::Global | SymbolScope::Builtin) {
                return Some(s);
            }
            return Some(self.define_free(s));
        }

        None
    }

    pub fn define(&mut self, key: &str) -> Rc<Symbol> {
        let symbol = Rc::new(Symbol {
            name: key.to_owned(),
            scope: match self.outer {
                Some(_) => SymbolScope::Local,
                None => SymbolScope::Global,
            },
            idx: self.n_defs,
        });

        self.store
            .borrow_mut()
            .insert(key.to_owned(), Rc::clone(&symbol));
        self.n_defs += 1;

        symbol
    }

    pub fn define_fn(&mut self, name: &str) -> Rc<Symbol> {
        let symbol = Rc::new(Symbol {
            name: name.to_owned(),
            scope: SymbolScope::Func,
            idx: 0,
        });
        self.store
            .borrow_mut()
            .insert(name.to_owned(), Rc::clone(&symbol));
        symbol
    }

    pub fn define_builtin(&mut self, idx: usize, key: String) -> Rc<Symbol> {
        let symbol = Rc::new(Symbol {
            name: key.clone(),
            scope: SymbolScope::Builtin,
            idx,
        });
        self.store
            .borrow_mut()
            .insert(key.clone(), Rc::clone(&symbol));
        symbol
    }

    pub fn define_free(&self, symbol: Rc<Symbol>) -> Rc<Symbol> {
        let mut free = self.free.borrow_mut();
        free.push(Rc::clone(&symbol));
        let idx = free.len() - 1;
        drop(free);

        let s = Rc::new(Symbol {
            name: symbol.name.clone(),
            scope: SymbolScope::Free,
            idx,
        });

        let mut store = self.store.borrow_mut();
        store.insert(symbol.name.clone(), Rc::clone(&s));
        drop(store);

        s
    }
}

impl Default for SymbolTable {
    fn default() -> Self {
        Self::new()
    }
}
