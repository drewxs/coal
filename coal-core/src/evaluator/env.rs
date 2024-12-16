use std::{cell::RefCell, collections::HashMap, rc::Rc};

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
