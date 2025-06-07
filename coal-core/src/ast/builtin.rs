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
                "void".into(),
                "bool".into(),
            ],
        },
    );
    builtins.insert(
        "assert_eq".to_owned(),
        TypeIdentifierTree {
            root: "fn".into(),
            children: vec!["any".into(), "any".into()],
        },
    );
    builtins.insert(
        "dbg".to_owned(),
        TypeIdentifierTree {
            root: "fn".into(),
            children: vec!["void".into(), "any".into()],
        },
    );
    builtins.insert(
        "print".to_owned(),
        TypeIdentifierTree {
            root: "fn".into(),
            children: vec!["void".into(), "any".into()],
        },
    );
    builtins.insert(
        "println".to_owned(),
        TypeIdentifierTree {
            root: "fn".into(),
            children: vec!["void".into(), "any".into()],
        },
    );

    builtins
}
