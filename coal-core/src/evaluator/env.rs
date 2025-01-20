use std::{
    cell::RefCell,
    collections::{hash_map::Entry, HashMap},
    rc::Rc,
};

use super::Object;

#[derive(Clone, Debug, PartialEq)]
pub struct Env {
    pub store: HashMap<String, Object>,
    pub outer: Option<Rc<RefCell<Env>>>,
}

impl Env {
    pub fn new(store: HashMap<String, Object>, outer: Rc<RefCell<Env>>) -> Self {
        Env {
            store,
            outer: Some(outer),
        }
    }

    pub fn has(&self, key: &str) -> bool {
        self.store.contains_key(key)
            || self
                .outer
                .as_ref()
                .and_then(|outer| outer.borrow().get(key))
                .is_some()
    }

    pub fn get(&self, key: &str) -> Option<Object> {
        self.store.get(key).cloned().or_else(|| {
            self.outer
                .as_ref()
                .and_then(|outer| outer.borrow().get(key))
        })
    }

    pub fn set_in_store(&mut self, key: String, value: Object) {
        self.store.insert(key, value);
    }

    pub fn set_in_scope(&mut self, key: String, value: Object) {
        match self.store.entry(key.clone()) {
            Entry::Occupied(mut entry) => {
                entry.insert(value);
            }
            Entry::Vacant(vacant_entry) => {
                if let Some(outer_env) = &self.outer {
                    if outer_env.borrow().has(&key) {
                        outer_env.borrow_mut().set_in_scope(key, value);
                        return;
                    }
                }
                vacant_entry.insert(value);
            }
        }
    }
}

impl Default for Env {
    fn default() -> Self {
        Env {
            store: HashMap::new(),
            outer: None,
        }
    }
}

impl From<Rc<RefCell<Env>>> for Env {
    fn from(outer: Rc<RefCell<Env>>) -> Self {
        Self {
            store: HashMap::new(),
            outer: Some(outer),
        }
    }
}
