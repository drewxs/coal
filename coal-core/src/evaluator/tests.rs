use std::{assert_matches::assert_matches, collections::HashMap};

use crate::{Expr, Ident, Infix, Param, Stmt, Type, F64, I32, U64};

use super::{Evaluator, Object, FALSE, TRUE};

fn check(tests: &[(&str, Object)]) {
    for (expr, expected) in tests {
        match Evaluator::default().eval(expr) {
            Ok(actual) => {
                if *expected != actual {
                    panic!(
                        "input:\n{}\nexpected:\n{:?}\nactual:\n{:?}",
                        expr, expected, actual
                    );
                }
            }
            Err(e) => {
                panic!("input:\n{}\nexpected:\n{}\nerror:\n{:?}", expr, expected, e);
            }
        }
    }
}

fn check_invalid(tests: &[&str]) {
    for input in tests {
        assert_matches!(Evaluator::default().eval(input), Err(_));
    }
}

#[test]
fn test_eval_literal() {
    check(&[
        ("5", Object::I32(5)),
        ("3.14", Object::F64(3.14)),
        ("true", TRUE),
        (r#""foo""#, Object::Str(String::from("foo"))),
    ]);
}

#[test]
fn test_eval_str_interpolation() {
    check(&[
        (
            r#""not {!false} or {!!false}""#,
            Object::Str(String::from("not true or false")),
        ),
        (
            r#""(1 + \"{-false})""#,
            Object::Str(String::from(r#"(1 + "0)"#)),
        ),
        (r#"let s = "asdf"; s.len()"#, Object::U64(4)),
        (r#""{1 + 9}".len()"#, Object::U64(2)),
    ]);
}

#[test]
fn test_eval_prefix() {
    check(&[
        ("-5", Object::I32(-5)),
        ("-10", Object::I32(-10)),
        ("!true", FALSE),
        ("!!true", TRUE),
        ("!false", TRUE),
        ("!!false", FALSE),
        ("!5", FALSE),
        ("!!5", TRUE),
    ]);
}

#[test]
fn test_eval_infix() {
    check(&[
        ("(7 + 2 * 3 / 2) % 3", Object::I32(1)),
        ("1 + 2 * 3 + 4 / 5", Object::I32(7)),
        ("(1 + 2 * 3 + 4 / 5) * 2 + -10", Object::I32(4)),
        ("true == true", TRUE),
        ("false == false", TRUE),
        ("true == false", FALSE),
        ("true != false", TRUE),
        ("(1 < 2) == true", TRUE),
        ("(1 > 2) != false", FALSE),
        (r#""foo" + "bar""#, Object::Str(String::from("foobar"))),
        (r#""foo" == "foo""#, TRUE),
        (r#""a" * 3"#, Object::Str(String::from("aaa"))),
        (r#""a" < "b""#, TRUE),
    ]);
}

#[test]
fn test_eval_infix_invalid() {
    check_invalid(&[
        r#""a" - "b""#,
        r#""a" / 3.14"#,
        r#"10 + "x""#,
        r#"10 - "x""#,
        r#"10 * "x""#,
        r#"10 / "x""#,
        r#"10 % "x""#,
        r#""x" + 10"#,
        r#""x" - 10"#,
        r#""x" / 10"#,
        r#""x" % 10"#,
    ]);
}

#[test]
fn test_eval_if() {
    check(&[
        ("if true { 10 }", Object::I32(10)),
        ("if false { 10 }", Object::Void),
        ("if 1 { 10 }", Object::I32(10)),
        ("if 1 < 2 { 10 }", Object::I32(10)),
        ("if 1 > 2 { 10 }", Object::Void),
        ("if 1 > 2 { 10 } else { 20 }", Object::I32(20)),
        ("if 1 < 2 { 10 } else { 20 }", Object::I32(10)),
        (
            r#"if 1 < 2 {
                let x = 999;
                return 10;
            } else {
                return 20;
            }"#,
            Object::I32(10),
        ),
        (
            r#"if 1 > 2 {
                return 1;
            } else {
                return 2;
                return 3;
            }"#,
            Object::I32(2),
        ),
    ]);
}

#[test]
fn test_eval_nested_if() {
    check(&[
        (
            r#"if true {
                if true {
                    return 1;
                }
                return 2;
            } else {
                return 3;
            }"#,
            Object::I32(1),
        ),
        (
            r#"if true {
                if false {
                    return 1;
                }
                return 2;
            } else {
                return 3;
            }"#,
            Object::I32(2),
        ),
    ]);
}

#[test]
fn test_eval_if_scope() {
    let input = r#"
        let x = 1;
        if true {
            let y = 2;
            let z = 3;
        }
    "#;

    let mut evaluator = Evaluator::default();
    let _ = evaluator.eval(input);
    let env = evaluator.env.borrow();

    assert_eq!(env.get("x"), Some(Object::I32(1)));
    assert_eq!(env.get("y"), None);
    assert_eq!(env.get("z"), None);
}

#[test]
fn test_eval_while_scope() {
    let input = r#"
        let i = 1;
        while i < 1000 {
            let x = 1;
            i = i + 1;
        }
    "#;

    let mut evaluator = Evaluator::default();
    let _ = evaluator.eval(input);
    let env = evaluator.env.borrow();

    assert_eq!(env.get("i"), Some(Object::I32(1000)));
    assert_eq!(env.get("x"), None);
}

#[test]
fn test_eval_let() {
    check(&[
        ("let x = 7; x;", Object::I32(7)),
        ("let x = 2 * 3; x;", Object::I32(6)),
        ("let x = 7; let y = 10; x;", Object::I32(7)),
        ("let x = 7; let y = 10; y;", Object::I32(10)),
        (
            "let x = 7; let y = 10; let z: i32 = x + y + 3; z;",
            Object::I32(20),
        ),
    ]);
}

#[test]
fn test_eval_let_scope() {
    check(&[
        (
            r#"
            let x = 1;
            if true {
                x = x + 1;
            };
            x;
            "#,
            Object::I32(2),
        ),
        (
            r#"
            let x = 1;
            fn foo() {
                x = x + 1;
            };
            foo();
            x;
            "#,
            Object::I32(2),
        ),
    ]);
}

#[test]
fn test_eval_assign() {
    check(&[
        ("let x = 1; x = 2; x;", Object::I32(2)),
        ("let x = 1; x = x + x + x; x;", Object::I32(3)),
        ("let x = 1; x += 1; x;", Object::I32(2)),
        ("let x = 4; x -= 1; x;", Object::I32(3)),
        ("let x = 2; x *= 3; x;", Object::I32(6)),
        ("let x = 8; x /= 2; x;", Object::I32(4)),
        ("let x = 10; x %= 3; x;", Object::I32(1)),
        (
            "let x = [1, 2]; x[0] = 3; x;",
            Object::List {
                data: vec![Object::I32(3), Object::I32(2)],
                t: I32,
            },
        ),
        (
            "let x = [1, 2]; x[0] += 6 / 2; x;",
            Object::List {
                data: vec![Object::I32(4), Object::I32(2)],
                t: I32,
            },
        ),
        (
            "let x = [[1, 2], [3, 4]]; x[0][1] = 6; x;",
            Object::List {
                data: vec![
                    Object::List {
                        data: vec![Object::I32(1), Object::I32(6)],
                        t: I32,
                    },
                    Object::List {
                        data: vec![Object::I32(3), Object::I32(4)],
                        t: I32,
                    },
                ],
                t: Type::List(Box::new(I32)),
            },
        ),
        (
            "let x = [[1, 2], [3, 4]]; x[0][1] += 3 * 2; x;",
            Object::List {
                data: vec![
                    Object::List {
                        data: vec![Object::I32(1), Object::I32(8)],
                        t: I32,
                    },
                    Object::List {
                        data: vec![Object::I32(3), Object::I32(4)],
                        t: I32,
                    },
                ],
                t: Type::List(Box::new(I32)),
            },
        ),
        (
            "let x = [[[1], [2]], [[3], [4]]]; x[0][1][0] = 6; x;",
            Object::List {
                data: vec![
                    Object::List {
                        data: vec![
                            Object::List {
                                data: vec![Object::I32(1)],
                                t: I32,
                            },
                            Object::List {
                                data: vec![Object::I32(6)],
                                t: I32,
                            },
                        ],
                        t: Type::List(Box::new(I32)),
                    },
                    Object::List {
                        data: vec![
                            Object::List {
                                data: vec![Object::I32(3)],
                                t: I32,
                            },
                            Object::List {
                                data: vec![Object::I32(4)],
                                t: I32,
                            },
                        ],
                        t: Type::List(Box::new(I32)),
                    },
                ],
                t: Type::List(Box::new(Type::List(Box::new(I32)))),
            },
        ),
    ]);
}

#[test]
fn test_eval_assign_invalid() {
    let tests = vec![
        "x = 1;",
        "fn x() -> i32 {}; x = 1;",
        "fn x() -> i32 { x = 1 }; x();",
        "fn x() -> i32 { y = 1 }; x();",
        "[1, 2][0] = true;",
        "[1, 2][2] = 0;",
        "let x = [1, 2]; x[0] = true;",
        "let x = [1, 2]; x[2] = 0;",
    ];
    for input in tests {
        assert_matches!(Evaluator::default().eval(input), Err(_));
    }
}

#[test]
fn test_eval_function() {
    check(&[(
        r#"fn add(x: i32, y: i32) -> i32 {
            if x > y {
                return x - y;
            } else {
                return x + y;
            }
        }"#,
        Object::Fn {
            name: String::from("add"),
            args: vec![Param::new("x", I32), Param::new("y", I32)],
            body: vec![Stmt::Expr(Expr::If {
                cond: Box::new(Expr::Infix(
                    Infix::GT,
                    Box::new(Expr::Ident(Ident::from("x"), I32, ((2, 4), (2, 4)))),
                    Box::new(Expr::Ident(Ident::from("y"), I32, ((2, 8), (2, 8)))),
                    ((2, 4), (2, 8)),
                )),
                then: vec![Stmt::Return(Expr::Infix(
                    Infix::Sub,
                    Box::new(Expr::Ident(Ident::from("x"), I32, ((3, 8), (3, 8)))),
                    Box::new(Expr::Ident(Ident::from("y"), I32, ((3, 12), (3, 12)))),
                    ((3, 8), (3, 12)),
                ))],
                elifs: vec![],
                alt: Some(vec![Stmt::Return(Expr::Infix(
                    Infix::Add,
                    Box::new(Expr::Ident(Ident::from("x"), I32, ((5, 8), (5, 8)))),
                    Box::new(Expr::Ident(Ident::from("y"), I32, ((5, 12), (5, 12)))),
                    ((5, 8), (5, 12)),
                ))]),
                span: ((2, 1), (6, 1)),
            })],
            ret_t: I32,
        },
    )]);
}

#[test]
fn test_eval_builtins() {
    check(&[
        (r#"print("asdf")"#, Object::Void),
        (r#"println("asdf")"#, Object::Void),
        (r#"dbg("asdf")"#, Object::Void),
    ]);
}

#[test]
fn test_eval_builtins_invalid() {
    check_invalid(&[
        r#"print()"#,
        r#"print(x, y)"#,
        r#"println()"#,
        r#"println(x, y, z)"#,
        r#"dbg()"#,
        r#"dbg(1, 2, 3)"#,
    ]);
}

#[test]
fn test_eval_lists() {
    check(&[
        (
            "[]",
            Object::List {
                data: vec![],
                t: Type::Unknown,
            },
        ),
        (
            "[1, 2, 3]",
            Object::List {
                data: vec![Object::I32(1), Object::I32(2), Object::I32(3)],
                t: I32,
            },
        ),
        (
            r#"["one", "two", "three"]"#,
            Object::List {
                data: vec![
                    Object::Str(String::from("one")),
                    Object::Str(String::from("two")),
                    Object::Str(String::from("three")),
                ],
                t: Type::Str,
            },
        ),
        (
            r#"
            fn f() -> str {
                return "asdf";
            }
            [f()];
            "#,
            Object::List {
                data: vec![Object::Str(String::from("asdf"))],
                t: Type::Str,
            },
        ),
        (
            r#"
            fn f() {}
            [f()];
            "#,
            Object::List {
                data: vec![],
                t: Type::Void,
            },
        ),
        ("[1, 2, 3].len()", Object::U64(3)),
        ("let x = [1, 2, 3]; x.push(7); x.len()", Object::U64(4)),
        ("[0, 10].pop()", Object::I32(10)),
        ("[0, 10].get(1)", Object::I32(10)),
        ("[1, 2, 3].first()", Object::I32(1)),
        ("[1, 2, 3].last()", Object::I32(3)),
        (r#"[1, 2, 3].join("-")"#, Object::Str(String::from("1-2-3"))),
        ("[1, 2, 3][1]", Object::I32(2)),
        (
            r#"
            fn f() {
                return [1, 2, 3];
            };
            f()[1]
            "#,
            Object::I32(2),
        ),
        (
            "[[1, 2, 3].len()]",
            Object::List {
                data: vec![Object::U64(3)],
                t: U64,
            },
        ),
        (
            "[[1, 2, 3][1], 1]",
            Object::List {
                data: vec![Object::I32(2), Object::I32(1)],
                t: I32,
            },
        ),
        (
            "let x = [1, 2, 3]; [x[-1], x[-2]]",
            Object::List {
                data: vec![Object::I32(3), Object::I32(2)],
                t: I32,
            },
        ),
        ("let x = [[1, 2], [3, 4]]; x[0][1]", Object::I32(2)),
        (
            "let x: list[i32] = []; x",
            Object::List {
                data: vec![],
                t: I32,
            },
        ),
    ]);
}

#[test]
fn test_eval_lists_invalid() {
    check_invalid(&["[f()]", "[print()]"]);
}

#[test]
fn test_eval_maps() {
    check(&[
        (
            "{}",
            Object::Map {
                data: HashMap::new(),
                t: (Type::Unknown, Type::Unknown),
            },
        ),
        (
            "let x: map[i32, i32] = {}; x",
            Object::Map {
                data: HashMap::new(),
                t: (I32, I32),
            },
        ),
        (
            "{1: 2}",
            Object::Map {
                data: HashMap::from([(Object::I32(1), Object::I32(2))]),
                t: (I32, I32),
            },
        ),
        (
            "let x: map[i32, i32] = {1: 2}; x",
            Object::Map {
                data: HashMap::from([(Object::I32(1), Object::I32(2))]),
                t: (I32, I32),
            },
        ),
        (
            "{1.2: 3}",
            Object::Map {
                data: HashMap::from([(Object::F64(1.2), Object::I32(3))]),
                t: (F64, I32),
            },
        ),
        (
            "let x = {1.2: 3}; x",
            Object::Map {
                data: HashMap::from([(Object::F64(1.2), Object::I32(3))]),
                t: (F64, I32),
            },
        ),
        (
            r#"{"one": 1}"#,
            Object::Map {
                data: HashMap::from([(Object::Str(String::from("one")), Object::I32(1))]),
                t: (Type::Str, I32),
            },
        ),
        (
            r#"let x = {"one": 1}; x"#,
            Object::Map {
                data: HashMap::from([(Object::Str(String::from("one")), Object::I32(1))]),
                t: (Type::Str, I32),
            },
        ),
    ]);
}

#[test]
fn test_eval_iter() {
    check(&[
        (
            r#"
            let x = 0;
            for i in 0..100 {
                x = i;
            }
            x
            "#,
            Object::I32(99),
        ),
        (
            r#"
            let x: u64 = 0;
            for i in 0..10 {
                x += i;
            }
            x
            "#,
            Object::U64(45),
        ),
        (
            r#"
            let x = 0.0;
            for i in 0..10 {
                x += i;
            }
            x
            "#,
            Object::F64(45.0),
        ),
        (
            r#"
            let count = 0;
            let list = [1, 2, 3];
            for x in list {
                count += x;
            }
            count
            "#,
            Object::I32(6),
        ),
    ]);
}

#[test]
fn test_eval_structs() {
    check(&[
        (
            r#"
            struct Foo {
                x: i32;
            }
            let foo = Foo { x: 1 };
            foo.x
            "#,
            Object::I32(1),
        ),
        (
            r#"
            struct Foo {
                x: i32;
                y: str = "bar";
            }
            let foo = Foo { x: 1 };
            foo.y
            "#,
            Object::Str(String::from("bar")),
        ),
        (
            r#"
            struct Foo {
                fn hello() -> str {
                    return "hello";
                }
            }
            let foo = Foo {};
            return foo.hello();
            "#,
            Object::Str(String::from("hello")),
        ),
        (
            r#"
            struct Foo {
                fn hello() -> str {
                    return "hello";
                }
                fn world() -> str {
                    return "world";
                }
            }
            let foo = Foo {};
            return foo.hello() + " " + foo.world();
            "#,
            Object::Str(String::from("hello world")),
        ),
    ]);
}

#[test]
fn test_eval_structs_invalid() {
    check_invalid(&[
        r#"
            struct Foo {
                x: i32;
                y: str;
            }
            Foo { x: 1 }
        "#,
        r#"
            struct Foo {
                x: i32;
                y: str;
            }
            Foo { x: 1, y: 2 }
        "#,
        r#"
            struct Foo {
                x: i32;
                y: str = "bar";
            }
            let foo = Foo { x: 1 };
            foo.z
        "#,
    ]);
}
