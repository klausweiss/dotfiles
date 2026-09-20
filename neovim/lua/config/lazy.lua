-- Bootstrap lazy.nvim
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

-- Make sure to setup `mapleader` and `maplocalleader` before
-- loading lazy.nvim so that mappings are correct.
-- This is also a good place to setup other settings (vim.opt)
vim.g.mapleader = " "
vim.g.maplocalleader = "\\"

-- Setup lazy.nvim
require("lazy").setup({
	spec = {
		-- import your plugins
		-- { import = "plugins" },
		--
		{
			"jakewvincent/mkdnflow.nvim",
			ft = { "markdown", "rmd" }, -- Add custom filetypes here if configured
			config = function()
				require("mkdnflow").setup({
					mappings = {
						MkdnFoldSection = false,
					},
				})
			end,
		},
		{
			"nvim-telescope/telescope.nvim",
			version = "*",
			dependencies = {
				"nvim-lua/plenary.nvim",
				-- optional but recommended
				{ "nvim-telescope/telescope-fzf-native.nvim", build = "make" },
			},
			keys = {
				{ "<leader>ff", "<cmd>Telescope find_files<cr>", desc = "Find files" },
			},
		},
		{
			"loctvl842/monokai-pro.nvim",
			version = "2.1.1",
			lazy = false,
			priority = 1000,
			config = function()
				require("monokai-pro").setup({
					filter = "spectrum",
				})
				vim.cmd.colorscheme("monokai-pro")
			end,
		},
		{
			"nvim-tree/nvim-tree.lua",
			dependencies = {
				"nvim-tree/nvim-web-devicons",
			},
			keys = {
				{ "<F1>", ":NvimTreeFindFileToggle<CR>" },
			},
			config = function()
				require("nvim-tree").setup({})

				-- Close nvim when nvim-tree is the last open window
				vim.api.nvim_create_autocmd("BufEnter", {
					nested = true,
					callback = function()
						if #vim.api.nvim_list_wins() == 1 and vim.bo.filetype == "NvimTree" then
							vim.cmd("quit")
						end
					end,
				})
			end,
		},
	},
	-- Configure any other settings here. See the documentation for more details.
	-- colorscheme that will be used when installing plugins.
	install = { colorscheme = { "habamax" } },
	-- automatically check for plugin updates
	checker = { enabled = true },
})
