local lazypath = vim.fn.stdpath("data") .. "/lazy/lazy.nvim"
if not (vim.uv or vim.loop).fs_stat(lazypath) then
  local lazyrepo = "https://github.com/folke/lazy.nvim.git"
  local out = vim.fn.system({ "git", "clone", "--filter=blob:none", "--branch=stable", lazyrepo, lazypath })
  if vim.v.shell_error ~= 0 then
    vim.api.nvim_echo({
      { "Failed to clone lazy.nvim:\n", "ErrorMsg" },
      { out, "WarningMsg" },
      { "\nPress any key to exit..." },
    }, true, {})
    vim.fn.getchar()
    os.exit(1)
  end
end
vim.opt.rtp:prepend(lazypath)

vim.g.mapleader = " "
vim.g.maplocalleader = "\\"
vim.opt.softtabstop = 4
vim.opt.shiftwidth = 4

require("lazy").setup({
  spec = {
    { "nvim-lua/plenary.nvim" },
    { "nvim-treesitter/nvim-treesitter", build = ":TSUpdate" },
    { "stevearc/conform.nvim" },
  },
  checker = { enabled = true },
})

require("nvim-treesitter.configs").setup({
  highlight = { enable = true },
  indent = { enable = true },
  ensure_installed = { "rust" },
})
vim.treesitter.language.register("rust", "coal")

require("conform").setup({
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
  format_on_save = {
    timeout_ms = 500,
    lsp_fallback = true,
  },
})
