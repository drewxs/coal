# Coal

A simple type-safe programming language.

Examples [here](/docs/examples.md).

> [!IMPORTANT]
> WIP; No 0.1.0 release yet, check back later!

## Installation

Install using [Cargo](https://www.rust-lang.org/tools/install):

```sh
cargo install --git https://github.com/drewxs/coal.git
```

## IDE support

### Neovim 

Register the filetype:

```lua
vim.filetype.add({
  extension = {
    coal = "coal",
  },
})
```

For syntax highlighting, add the following to your treesitter config:
Using Rust grammar (for now) as it's the closest thing to Coal's syntax.

```lua
vim.treesitter.language.register("rust", "coal")
```

### VSCode

> ¯\_(ツ)_/¯

## Development

Prerequisites:

- [Rust](https://www.rust-lang.org/tools/install)
- [GNU Make](https://www.gnu.org/software/make)

Run `make` to print a list of available commands:

---

[License](https://github.com/drewxs/coal/blob/main/LICENSE)
