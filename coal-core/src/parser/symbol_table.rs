use std::{cell::RefCell, collections::HashMap, rc::Rc};

use crate::{BaseType, Num, ResolvedType, TypeIdentifier, TypeIdentifierTree, builtin_types};

#[derive(Clone, Debug, PartialEq)]
pub struct TypeSymbolTable {
    pub scope: String,
    // key: Cat
    // value: Struct { "Cat", ["name", (string type)], [meow func] }
    pub store: RefCell<HashMap<TypeIdentifier, BaseType>>,
    pub outer: Option<Rc<RefCell<TypeSymbolTable>>>,
}

impl TypeSymbolTable {
    pub fn new(
        store: HashMap<TypeIdentifier, BaseType>,
        outer: Rc<RefCell<TypeSymbolTable>>,
    ) -> Self {
        TypeSymbolTable {
            scope: String::from("global"),
            store: RefCell::new(store),
            outer: Some(outer),
        }
    }

    pub fn get(&self, key: &str) -> Option<BaseType> {
        self.store.borrow().get(key).cloned().or_else(|| {
            self.outer
                .as_ref()
                .and_then(|outer| outer.borrow().get(key))
        })
    }

    pub fn set(&self, key: String, value: BaseType) {
        self.store.borrow_mut().insert(key, value);
    }

    pub fn resolve(&self, type_identifier_tree: &TypeIdentifierTree) -> Option<ResolvedType> {
        let resolved_children: Vec<ResolvedType> = type_identifier_tree
            .children
            .iter()
            .map(|child| self.resolve(child))
            .collect::<Option<Vec<ResolvedType>>>()?;

        // TODO: factor out builtin types + arities to a separate file
        match (type_identifier_tree.root.as_str(), resolved_children.len()) {
            ("list", 1) => Some(ResolvedType {
                base: BaseType::List,
                args: vec![resolved_children[0].clone()],
            }),
            ("map", 2) => Some(ResolvedType {
                base: BaseType::Map,
                args: vec![resolved_children[0].clone(), resolved_children[1].clone()],
            }),
            ("fn", _) => Some(ResolvedType {
                base: BaseType::Fn,
                args: resolved_children,
            }),
            (ident, _) => self.get(ident).map(|base| ResolvedType {
                base,
                args: resolved_children,
            }),
        }
    }
}

impl Default for TypeSymbolTable {
    fn default() -> Self {
        TypeSymbolTable {
            scope: String::from("global"),
            store: RefCell::new(HashMap::from([
                ("bool".to_string(), BaseType::Bool),
                ("str".to_string(), BaseType::Str),
                ("u32".to_string(), BaseType::Num(Num::U32)),
                ("u64".to_string(), BaseType::Num(Num::U64)),
                ("i32".to_string(), BaseType::Num(Num::I32)),
                ("i64".to_string(), BaseType::Num(Num::I64)),
                ("i128".to_string(), BaseType::Num(Num::I128)),
                ("f32".to_string(), BaseType::Num(Num::F32)),
                ("f64".to_string(), BaseType::Num(Num::F64)),
                ("range".to_string(), BaseType::Range),
                ("nil".to_string(), BaseType::Nil),
                ("any".to_string(), BaseType::Any),
                ("void".to_string(), BaseType::Void),
                ("unknown".to_string(), BaseType::Unknown),
            ])),
            outer: None,
        }
    }
}

impl From<Rc<RefCell<TypeSymbolTable>>> for TypeSymbolTable {
    fn from(outer: Rc<RefCell<TypeSymbolTable>>) -> Self {
        let TypeSymbolTable { scope, .. } = outer.borrow().clone();
        Self {
            scope,
            store: RefCell::new(HashMap::new()),
            outer: Some(outer),
        }
    }
}

/// Keys of shape `__name__` are return types for functions
#[derive(Clone, Debug, PartialEq)]
pub struct SymbolTable {
    pub scope: String,
    // key: mycats: List
    pub store: RefCell<HashMap<String, TypeIdentifierTree>>,
    pub type_symbol_table: Rc<RefCell<TypeSymbolTable>>,
    pub outer: Option<Rc<RefCell<SymbolTable>>>,
}

impl SymbolTable {
    pub fn new(
        store: HashMap<String, TypeIdentifierTree>,
        outer: Rc<RefCell<SymbolTable>>,
    ) -> Self {
        SymbolTable {
            scope: String::from("global"),
            store: RefCell::new(store),
            type_symbol_table: Rc::new(RefCell::new(TypeSymbolTable {
                scope: String::from("global"),
                store: RefCell::new(HashMap::new()),
                outer: Some(Rc::clone(&outer.borrow().type_symbol_table)),
            })),
            outer: Some(Rc::clone(&outer)),
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

    pub fn get(&self, key: &str) -> Option<ResolvedType> {
        self.store
            .borrow()
            .get(key)
            .and_then(|v| self.type_symbol_table.borrow().resolve(v))
            .or_else(|| {
                self.outer
                    .as_ref()
                    .and_then(|outer| outer.borrow().get(key))
            })
    }

    pub fn get_ret_t(&self, key: &str) -> Option<ResolvedType> {
        self.get(key).and_then(|resolved_type| {
            resolved_type
                .args
                .first()
                .cloned()
                .or_else(|| Some(ResolvedType::default()))
        })
    }

    pub fn set(&self, key: String, value: ResolvedType) {
        self.store.borrow_mut().insert(key, value.into());
    }
}

impl Default for SymbolTable {
    fn default() -> Self {
        SymbolTable {
            scope: String::from("global"),
            store: RefCell::new(builtin_types()),
            type_symbol_table: Rc::new(RefCell::new(TypeSymbolTable::default())),
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
            type_symbol_table: Rc::new(RefCell::new(TypeSymbolTable::from(Rc::clone(
                &outer.borrow().type_symbol_table,
            )))),
            outer: Some(Rc::clone(&outer)),
        }
    }
}
