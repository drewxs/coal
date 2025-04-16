use std::{cell::RefCell, collections::HashMap, rc::Rc};

use crate::{Type, builtin_types};

/// Keys of shape `__name__` are return types for functions
#[derive(Clone, Debug, PartialEq)]
pub struct SymbolTable {
    pub scope: String,
    pub store: RefCell<HashMap<String, Type>>,
    pub outer: Option<Rc<RefCell<SymbolTable>>>,
}

impl SymbolTable {
    pub fn new(store: HashMap<String, Type>, outer: Rc<RefCell<SymbolTable>>) -> Self {
        SymbolTable {
            scope: String::from("global"),
            store: RefCell::new(store),
            outer: Some(outer),
        }
    }

    pub fn has(&self, key: &str) -> bool {
        self.store.borrow().contains_key(key)
            || self
                .outer
                .as_ref()
                .map(|outer| outer.borrow().has(key))
                .is_some()
    }

    pub fn get(&self, key: &str) -> Option<Type> {
        self.store.borrow().get(key).cloned().or_else(|| {
            self.outer
                .as_ref()
                .and_then(|outer| outer.borrow().get(key))
        })
    }

    pub fn get_ret_t(&self, key: &str) -> Option<Type> {
        self.get(&format!("__{key}__"))
    }

    pub fn set(&self, key: String, value: Type) {
        self.store.borrow_mut().insert(key, value);
    }

    pub fn set_ret_t(&self, key: String, value: Type) {
        self.set(format!("__{key}__"), value);
    }
}

impl Default for SymbolTable {
    fn default() -> Self {
        SymbolTable {
            scope: String::from("global"),
            store: RefCell::new(builtin_types()),
            outer: None,
        }
    }
}

impl From<Rc<RefCell<SymbolTable>>> for SymbolTable {
    fn from(outer: Rc<RefCell<SymbolTable>>) -> Self {
        let SymbolTable { scope, .. } = outer.borrow().clone();
        Self {
            scope,
            store: RefCell::new(builtin_types()),
            outer: Some(outer),
        }
    }
}
