use std::{
    cell::RefCell,
    collections::{HashMap, hash_map::Entry},
    rc::Rc,
};

use coal_objects::Object;

#[derive(Clone, Debug, PartialEq, Default)]
pub struct Env {
    pub store: RefCell<HashMap<String, Object>>,
    pub outer: Option<Rc<RefCell<Env>>>,
}

impl Env {
    pub fn new(store: HashMap<String, Object>, outer: Rc<RefCell<Env>>) -> Env {
        Env {
            store: RefCell::new(store),
            outer: Some(outer),
        }
    }

    pub fn new_enclosed(outer: Rc<RefCell<Env>>) -> Rc<RefCell<Env>> {
        Rc::new(RefCell::new(Env {
            store: RefCell::new(HashMap::new()),
            outer: Some(outer),
        }))
    }

    pub fn has(&self, key: &str) -> bool {
        if self.store.borrow().contains_key(key) {
            return true;
        }
        if let Some(outer) = &self.outer {
            return outer.borrow().has(key);
        }
        false
    }

    pub fn get(&self, key: &str) -> Option<Object> {
        if let Some(val) = self.store.borrow().get(key).cloned() {
            return Some(val);
        }
        self.outer
            .as_ref()
            .and_then(|outer| outer.borrow().get(key))
    }

    pub fn set_local(&self, key: String, value: Object) {
        self.store.borrow_mut().insert(key, value);
    }

    pub fn set(&self, key: String, value: Object) {
        match self.store.borrow_mut().entry(key.clone()) {
            Entry::Occupied(mut entry) => {
                entry.insert(value);
            }
            Entry::Vacant(vacant_entry) => {
                if let Some(outer_env) = &self.outer {
                    if outer_env.borrow().has(&key) {
                        outer_env.borrow().set(key, value);
                        return;
                    }
                }
                vacant_entry.insert(value);
            }
        }
    }
}

impl From<Rc<RefCell<Env>>> for Env {
    fn from(outer: Rc<RefCell<Env>>) -> Self {
        Self {
            store: RefCell::new(HashMap::new()),
            outer: Some(outer),
        }
    }
}
