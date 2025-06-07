use std::collections::HashMap;

use super::TypeIdentifierTree;

pub fn builtin_types() -> HashMap<String, TypeIdentifierTree> {
    let mut builtins = HashMap::new();

    builtins.insert(
        "assert".to_owned(),
        TypeIdentifierTree {
            root: "fn".into(),
            children: vec![
                // return value comes first for functions in type identifier trees
                TypeIdentifierTree {
                    root: "void".into(),
                    children: vec![],
                },
                TypeIdentifierTree {
                    root: "bool".into(),
                    children: vec![],
                },
            ],
        },
    );
    builtins.insert(
        "assert_eq".to_owned(),
        TypeIdentifierTree {
            root: "fn".into(),
            children: vec![
                TypeIdentifierTree {
                    root: "any".into(),
                    children: vec![],
                },
                TypeIdentifierTree {
                    root: "any".into(),
                    children: vec![],
                },
            ],
        },
    );
    builtins.insert(
        "dbg".to_owned(),
        TypeIdentifierTree {
            root: "fn".into(),
            children: vec![
                TypeIdentifierTree {
                    root: "void".into(),
                    children: vec![],
                },
                TypeIdentifierTree {
                    root: "any".into(),
                    children: vec![],
                },
            ],
        },
    );
    builtins.insert(
        "print".to_owned(),
        TypeIdentifierTree {
            root: "fn".into(),
            children: vec![
                TypeIdentifierTree {
                    root: "void".into(),
                    children: vec![],
                },
                TypeIdentifierTree {
                    root: "any".into(),
                    children: vec![],
                },
            ],
        },
    );
    builtins.insert(
        "println".to_owned(),
        TypeIdentifierTree {
            root: "fn".into(),
            children: vec![
                TypeIdentifierTree {
                    root: "void".into(),
                    children: vec![],
                },
                TypeIdentifierTree {
                    root: "any".into(),
                    children: vec![],
                },
            ],
        },
    );

    builtins
}
