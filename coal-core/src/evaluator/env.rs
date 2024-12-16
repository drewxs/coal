use std::{cell::RefCell, collections::HashMap, rc::Rc};

use super::Object;

#[derive(Clone, Debug, PartialEq, Default)]
pub struct Env {
    store: HashMap<String, Object>,
    outer: Option<Rc<RefCell<Env>>>,
}

impl Env {
    pub fn new(store: HashMap<String, Object>, outer: Option<Rc<RefCell<Env>>>) -> Self {
        Env { store, outer }
    }

    pub fn get(&self, key: &str) -> Option<Object> {
        self.store.get(key).cloned().or_else(|| {
            self.outer
                .as_ref()
                .and_then(|outer| outer.borrow().get(key))
        })
    }

    pub fn set(&mut self, name: String, value: Object) {
        self.store.insert(name, value);
    }

    pub fn extend(&mut self, other: Env) {
        self.store.extend(other.store);
    }
}

impl From<HashMap<String, Object>> for Env {
    fn from(store: HashMap<String, Object>) -> Self {
        Self { store, outer: None }
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
