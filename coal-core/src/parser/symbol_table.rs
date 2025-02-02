use std::{cell::RefCell, collections::HashMap, rc::Rc};

use crate::{builtins, Type};

/// Keys of shape `__name__` are return types for functions
#[derive(Clone, Debug, PartialEq)]
pub struct SymbolTable {
    pub scope: String,
    pub store: HashMap<String, Type>,
    pub outer: Option<Rc<RefCell<SymbolTable>>>,
}

impl SymbolTable {
    pub fn new(store: HashMap<String, Type>, outer: Rc<RefCell<SymbolTable>>) -> Self {
        SymbolTable {
            scope: String::from("global"),
            store,
            outer: Some(outer),
        }
    }

    pub fn has(&self, key: &str) -> bool {
        self.store.contains_key(key)
            || self
                .outer
                .as_ref()
                .map(|outer| outer.borrow().has(key))
                .is_some()
    }

    pub fn get(&self, key: &str) -> Option<Type> {
        self.store.get(key).cloned().or_else(|| {
            self.outer
                .as_ref()
                .and_then(|outer| outer.borrow().get(key))
        })
    }

    pub fn get_ret_t(&self, key: &str) -> Option<Type> {
        self.get(&format!("__{}__", key))
    }

    pub fn set(&mut self, key: String, value: Type) {
        self.store.insert(key, value);
    }

    pub fn set_ret_t(&mut self, key: String, value: Type) {
        self.set(format!("__{}__", key), value);
    }
}

impl Default for SymbolTable {
    fn default() -> Self {
        SymbolTable {
            scope: String::from("global"),
            store: builtins::types(),
            outer: None,
        }
    }
}

impl From<Rc<RefCell<SymbolTable>>> for SymbolTable {
    fn from(outer: Rc<RefCell<SymbolTable>>) -> Self {
        let SymbolTable { scope, .. } = outer.borrow().clone();
        Self {
            scope,
            store: builtins::types(),
            outer: Some(outer),
        }
    }
}
