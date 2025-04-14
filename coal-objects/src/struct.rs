use std::hash::{Hash, Hasher};

use super::Object;

#[derive(Clone, Debug, PartialEq)]
pub struct Struct {
    pub name: String,
    pub attrs: Vec<(String, Object)>,
    pub funcs: Vec<(String, Object)>,
}

impl Struct {
    pub fn new(name: &str, attrs: &[(String, Object)], funcs: &[(String, Object)]) -> Self {
        Struct {
            name: name.to_owned(),
            attrs: attrs.to_owned(),
            funcs: funcs.to_owned(),
        }
    }

    pub fn get(&self, key: &str) -> Option<&Object> {
        self.attrs
            .iter()
            .chain(self.funcs.iter())
            .find(|(attr, _)| attr == key)
            .map(|(_, v)| v)
    }

    pub fn set(&mut self, keys: &[String], val: Object) {
        match keys.len() {
            0 => {}
            1 => {
                if let Some((_, v)) = self
                    .attrs
                    .iter_mut()
                    .chain(self.funcs.iter_mut())
                    .find(|(attr, _)| attr == keys.first().unwrap())
                {
                    *v = val;
                }
            }
            _ => {
                let mut curr = self;
                for key in keys.iter().take(keys.len() - 1) {
                    if let Some((_, Object::Struct(next_struct))) =
                        curr.attrs.iter_mut().find(|(k, _)| k == key)
                    {
                        curr = next_struct;
                    } else {
                        return;
                    }
                }

                if let Some((_, obj)) = curr
                    .attrs
                    .iter_mut()
                    .find(|(k, _)| k == keys.last().unwrap())
                {
                    *obj = val;
                }
            }
        }
    }
}

impl Hash for Struct {
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.name.hash(state);
        self.attrs.len().hash(state);
        for (k, v) in &self.attrs {
            k.hash(state);
            v.hash(state);
        }
        self.funcs.len().hash(state);
        for (k, v) in &self.funcs {
            k.hash(state);
            v.hash(state);
        }
    }
}
