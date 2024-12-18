use std::{
    cell::RefCell,
    collections::{hash_map::Entry, HashMap},
    rc::Rc,
};

use super::Object;

#[derive(Clone, Debug, PartialEq, Default)]
pub struct Env {
    store: HashMap<String, Object>,
    outer: Option<Rc<RefCell<Env>>>,
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

    pub fn set(&mut self, name: String, value: Object) {
        match self.store.entry(name.clone()) {
            Entry::Occupied(mut entry) => {
                entry.insert(value);
            }
            Entry::Vacant(vacant_entry) => {
                if let Some(outer_env) = &self.outer {
                    if outer_env.borrow().has(&name) {
                        outer_env.borrow_mut().set(name, value);
                        return;
                    }
                }
                vacant_entry.insert(value);
            }
        }
    }

    pub fn extend(&mut self, other: Rc<RefCell<Env>>) {
        self.store.extend(other.borrow().store.clone());
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
