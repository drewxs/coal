use super::Object;

#[derive(Clone, Debug, PartialEq)]
pub struct StructObj {
    pub name: String,
    pub attrs: Vec<(String, Object)>,
    pub funcs: Vec<(String, Object)>,
}

impl StructObj {
    pub fn new(name: &str, attrs: &[(String, Object)], funcs: &[(String, Object)]) -> Self {
        StructObj {
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

    pub fn set(&mut self, key: &str, val: Object) {
        if let Some((_, v)) = self
            .attrs
            .iter_mut()
            .chain(self.funcs.iter_mut())
            .find(|(attr, _)| attr == key)
        {
            *v = val;
        }
    }
}
