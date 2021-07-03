local cmd = vim.cmd

-- theme, powerline
vim.api.nvim_set_var('gruvbox_material_sign_column_background', 'clear') -- transparent signcolumn
cmd 'autocmd vimenter * ++nested colorscheme gruvbox-material'
vim.api.nvim_set_var('airline_powerline_fonts', 1)
-- line numbers
cmd 'set number'


