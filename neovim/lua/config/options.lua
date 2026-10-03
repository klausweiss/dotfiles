-- Case-insensitive search, unless the pattern contains an uppercase letter
vim.opt.ignorecase = true
vim.opt.smartcase = true

-- Indent with 4 spaces (filetype plugins may still override, e.g. Makefiles keep real tabs)
vim.opt.tabstop = 4
vim.opt.shiftwidth = 4
vim.opt.softtabstop = 4
