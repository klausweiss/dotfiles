return {
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
}
