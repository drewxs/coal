# Examples

Hello, World!:

```rs
print("Hello, World!");
```

Variables:

```rs
let i: int = 10;
let f: float = 3.14;
let s: str = "Hello";
let b: bool = true;
```

Vectors:

```rs
let x: list[int] = [1, 2, 3];

x.len(); // 3
x.first() // 1
x.last() // 3
x.push(4) // [1, 2, 3, 4]
```

Hashes:

```rs
let x: map[int, str] = { 1: "one", 2: "two" };

x[1]; // "one"
x[2]; // "two"
x[4] = "four"; // { 1: "one", 2: "two", 4: "four" }
```

Functions:

```rs
fn x(a: int, b: int) -> int {
    return a + b;
}

x(1, 2); // 3
```
