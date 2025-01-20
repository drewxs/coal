# Coal

A simple type-safe programming language.

Examples [here](/docs/examples.md).

> [!IMPORTANT]
> WIP; No 0.1.0 release yet, check back later!

## Installation

Install using [Cargo](https://www.rust-lang.org/tools/install) (requires nightly rust):

```sh
cargo install --git https://github.com/drewxs/coal.git
```

Build from source (also requires nightly rust):

```sh
git clone https://github.com/drewxs/coal.git
cd coal
make
```

## Usage

```sh
# Start the REPL
coal

# Create a new project
coal new PROJECT_NAME

# Run the current project or a given program
coal run [PROGRAM_PATH]

# Evaluate a string
coal eval STRING

# Format all files in the current project or from a given path
coal fmt [PATH]
```

Run `coal --help` for more information.

## IDE support

### Neovim

#### Filetype detection

Add anywhere that's loaded by `init.lua`:

```lua
vim.filetype.add({
  extension = {
    coal = "coal",
  },
})
```

#### Syntax highlighting

Add to your treesitter config:

```lua
vim.treesitter.language.register("rust", "coal")
```

> Using Rust grammar (for now) as it's the closest thing to Coal's syntax.

#### Formatting

Using no plugins:

```lua
vim.api.nvim_create_autocmd("BufWritePre", {
  pattern = "*.coal",
  callback = function()
    local view = vim.fn.winsaveview()
    vim.cmd("%!coal fmt")
    vim.fn.winrestview(view)
  end,
})
```

> This is basic and won't handle things like preserving cursor positions when formatting extra newlines.
> For a more robust solution, you can use a formatter plugin like [conform.nvim](https://github.com/stevearc/conform.nvim).

Using [conform.nvim](https://github.com/stevearc/conform.nvim):

Add the following to your conform config:

```lua
{
  formatters = {
    coal_fmt = {
      command = "coal",
      args = { "fmt" },
      stdin = true,
    },
  },
  formatters_by_ft = {
    coal = { "coal_fmt" },
  },
}
```

### VSCode

¯\\\_(ツ)\_/¯

## Development

Prerequisites:

- [Rust](https://www.rust-lang.org/tools/install)
- [GNU Make](https://www.gnu.org/software/make)

Run `make help` to print a list of available commands:

---

[Apache License 2.0](https://github.com/drewxs/coal/blob/main/LICENSE)
