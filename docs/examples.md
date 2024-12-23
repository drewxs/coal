# Examples

Hello, World!:

```rs
println("Hello, World!");
```

Variables:

```rs
let i: i32 = 10;
let f: f64 = 3.14;
let s: str = "Hello";
let b: bool = true;
```

Vectors:

```rs
let x: list[i32] = [1, 2, 3];

x.len(); // 3
x.first() // 1
x.last() // 3
x.push(4) // [1, 2, 3, 4]
```

Hashes:

```rs
let x: map[i32, str] = { 1: "one", 2: "two" };

x[1]; // "one"
x[2]; // "two"
x[4] = "four"; // { 1: "one", 2: "two", 4: "four" }
```

Functions:

```rs
fn x(a: i32, b: i32) -> i32 {
    return a + b;
}

x(1, 2); // 3
```
