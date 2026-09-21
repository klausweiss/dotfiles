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
				{ "<leader>fp", "<cmd>Telescope commands<cr>", desc = "Command palette" },
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
			-- Load eagerly so it can hijack directory buffers (`nvim .`)
			lazy = false,
			keys = {
				{ "<F1>", ":NvimTreeFindFileToggle<CR>" },
			},
			init = function()
				-- Recommended by nvim-tree: disable netrw so it doesn't race for directories
				vim.g.loaded_netrw = 1
				vim.g.loaded_netrwPlugin = 1
			end,
			config = function()
				require("nvim-tree").setup({
					hijack_directories = { enable = true, auto_open = true },
				})

				-- Close nvim when nvim-tree is the last open window
				vim.api.nvim_create_autocmd("BufEnter", {
					nested = true,
					callback = function()
						-- vim_did_enter guard: `nvim .` starts with the tree as the only window
						if vim.v.vim_did_enter ~= 1 then
							return
						end
						-- Deferred so that commands that briefly leave the tree alone
						-- (e.g. Telescope closing its popup before `:edit`) can finish first
						vim.schedule(function()
							if #vim.api.nvim_list_wins() == 1 and vim.bo.filetype == "NvimTree" then
								vim.cmd("quit")
							end
						end)
					end,
				})
			end,
		},
		{
			"obsidian-nvim/obsidian.nvim",
			version = "*", -- use latest release, remove to use latest commit
			-- obsidian.nvim's UI features need conceallevel 1 or 2
			init = function()
				vim.api.nvim_create_autocmd("FileType", {
					pattern = "markdown",
					callback = function()
						vim.opt_local.conceallevel = 1
					end,
				})
			end,
			---@module 'obsidian'
			---@type obsidian.config
			opts = {
				legacy_commands = false, -- this will be removed in 4.0.0
				workspaces = {
					{
						-- Dynamic workspace: <project>/wiki/src, found by walking up from the
						-- current buffer (falling back to the cwd).
						name = "project-wiki",
						path = function()
							local buf_dir = vim.fs.dirname(vim.api.nvim_buf_get_name(0))
							local start = (buf_dir and buf_dir ~= "") and buf_dir or vim.uv.cwd()
							for dir in vim.fs.parents(start .. "/") do
								local wiki = vim.fs.joinpath(dir, "wiki", "src")
								if vim.fn.isdirectory(wiki) == 1 then
									return wiki
								end
							end
							return start
						end,
						overrides = {
							-- daily/2026-09/2026-09-21.md
							daily_notes = {
								folder = "daily",
								date_format = "YYYY-MM/YYYY-MM-DD",
							},
						},
					},
				},
			},
		},
		{
			"saghen/blink.cmp",
			-- optional: provides snippets for the snippet source
			dependencies = { "rafamadriz/friendly-snippets" },

			-- use a release tag to download pre-built binaries
			version = "1.*",
			-- AND/OR build from source
			-- build = 'cargo build --release',
			-- If you use nix, you can build from source with:
			-- build = 'nix run .#build-plugin',

			---@module 'blink.cmp'
			---@type blink.cmp.Config
			opts = {
				-- 'default' (recommended) for mappings similar to built-in completions (C-y to accept)
				-- 'super-tab' for mappings similar to vscode (tab to accept)
				-- 'enter' for enter to accept
				-- 'none' for no mappings
				--
				-- All presets have the following mappings:
				-- C-space: Open menu or open docs if already open
				-- C-n/C-p or Up/Down: Select next/previous item
				-- C-e: Hide menu
				-- C-k: Toggle signature help (if signature.enabled = true)
				--
				-- See :h blink-cmp-config-keymap for defining your own keymap
				keymap = { preset = "default" },

				appearance = {
					-- 'mono' (default) for 'Nerd Font Mono' or 'normal' for 'Nerd Font'
					-- Adjusts spacing to ensure icons are aligned
					nerd_font_variant = "mono",
				},

				-- (Default) Only show the documentation popup when manually triggered
				completion = { documentation = { auto_show = false } },

				-- Default list of enabled providers defined so that you can extend it
				-- elsewhere in your config, without redefining it, due to `opts_extend`
				sources = {
					default = { "lsp", "path", "snippets", "buffer" },
				},

				-- (Default) Rust fuzzy matcher for typo resistance and significantly better performance
				-- You may use a lua implementation instead by using `implementation = "lua"` or fallback to the lua implementation,
				-- when the Rust fuzzy matcher is not available, by using `implementation = "prefer_rust"`
				--
				-- See the fuzzy documentation for more information
				fuzzy = { implementation = "prefer_rust_with_warning" },
			},
			opts_extend = { "sources.default" },
		},
	},
	-- Configure any other settings here. See the documentation for more details.
	-- colorscheme that will be used when installing plugins.
	install = { colorscheme = { "habamax" } },
	-- automatically check for plugin updates
	checker = { enabled = true },
})
