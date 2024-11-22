# Examples

Hello, World!:

```rs
print("Hello, World!");
```

Variables:

```rs
let some_num: int = 10;
let some_string: str = "Hello";
let some_bool: bool = true;
let some_list: list[int] = [1, 2, 3];
let some_map: map[int, str] = { 1: "one", 2: "two", 3: "three" };
fn some_fn(x) {
    return x + 1;
}
```

Vectors:

```rs
let x: list[int] = [1, 2, 3];
x.len(); // 3
print(x) // [1, 2, 3]
x.first() // 1
x.last() // 3
x.push(4) // [1, 2, 3, 4]
```

Hashes:

```rs
let x: map[int, str] = { 1: "one", 2: "two" };
x[1]; // "one"
x[2]; // "two"
fn double(x) {
    return x * 2;
}
x[double(2)] = "four"; // { 1: "one", 2: "two", 4: "four" }
```

Functions:

```rs
fn x(a, b) {
  return a + b;
}
x(1, 2); // 3
```
