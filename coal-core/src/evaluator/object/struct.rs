use super::Object;

#[derive(Clone, Debug, PartialEq)]
pub struct StructObj {
    pub name: String,
    pub attrs: Vec<(String, Object)>,
}

impl StructObj {
    pub fn new(name: &str, attrs: &[(String, Object)]) -> Self {
        StructObj {
            name: name.to_owned(),
            attrs: attrs.to_owned(),
        }
    }

    pub fn get(&self, key: &str) -> Option<&Object> {
        self.attrs
            .iter()
            .find(|(attr, _)| attr == key)
            .map(|(_, v)| v)
    }
}
