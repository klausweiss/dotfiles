return {
	{
		"neovim/nvim-lspconfig",
		-- Only ships server configs (lsp/*.lua) on the runtimepath, so it's cheap to load eagerly
		lazy = false,
		config = function()
			-- Completion capabilities are registered by blink.cmp via vim.lsp.config("*", ...).
			-- Default keymaps (Neovim 0.11+): K hover, grn rename, gra code action,
			-- grr references, gri implementation, gO document symbols, <C-s> signature help (insert)
			vim.diagnostic.config({ virtual_text = true })

			vim.lsp.config("basedpyright", {
				-- Fetched and cached by uv, no global install needed. The PyPI package
				-- bundles its own Node.js, so this works without node on the PATH.
				cmd = { "uvx", "--from", "basedpyright", "basedpyright-langserver", "--stdio" },
				-- Use the project's uv-managed virtualenv, so its dependencies resolve
				before_init = function(_, config)
					local python = vim.fs.joinpath(config.root_dir, ".venv", "bin", "python")
					if vim.uv.fs_stat(python) then
						config.settings.python = vim.tbl_deep_extend("force", config.settings.python or {}, {
							pythonPath = python,
						})
					end
				end,
				settings = {
					basedpyright = {
						-- basedpyright defaults to the much noisier "recommended"
						analysis = { typeCheckingMode = "standard" },
					},
				},
			})

			vim.lsp.enable({
				"basedpyright", -- Python
				"hls", -- Haskell
				"rust_analyzer", -- Rust
				"clangd", -- C / C++
				"ts_ls", -- TypeScript / JavaScript
				"terraformls", -- Terraform
				"marksman", -- Markdown
			})
		end,
	},
}
