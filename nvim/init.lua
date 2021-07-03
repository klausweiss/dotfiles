local cmd = vim.cmd

-- theme, powerline
cmd "autocmd vimenter * ++nested colorscheme gruvbox"
vim.api.nvim_set_var('airline_powerline_fonts', 1)


