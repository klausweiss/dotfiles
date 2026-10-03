local last_win

return {
	{
		"nvim-tree/nvim-tree.lua",
		dependencies = {
			"nvim-tree/nvim-web-devicons",
		},
		-- Load eagerly so it can hijack directory buffers (`nvim .`)
		lazy = false,
		keys = {
			{
				"<F1>",
				function()
					local api = require("nvim-tree.api")
					if vim.bo.filetype == "NvimTree" then
						-- Tree is focused: jump back to where we came from
						if last_win and vim.api.nvim_win_is_valid(last_win) then
							vim.api.nvim_set_current_win(last_win)
						else
							vim.cmd("wincmd p")
						end
					else
						-- Tree is closed or unfocused: open/focus it on the current file
						last_win = vim.api.nvim_get_current_win()
						api.tree.find_file({ open = true, focus = true })
					end
				end,
				desc = "Focus nvim-tree / return to code",
			},
			{
				"<S-F1>",
				function()
					require("nvim-tree.api").tree.close()
				end,
				desc = "Close nvim-tree",
			},
			-- Many terminals send Shift-F1 as F13
			{
				"<F13>",
				function()
					require("nvim-tree.api").tree.close()
				end,
				desc = "Close nvim-tree",
			},
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
						-- Ignore the minimap, which can also be left over (and focused)
						local wins = vim.tbl_filter(function(win)
							return vim.bo[vim.api.nvim_win_get_buf(win)].filetype ~= "minimap"
						end, vim.api.nvim_list_wins())
						if #wins == 1 and vim.bo[vim.api.nvim_win_get_buf(wins[1])].filetype == "NvimTree" then
							vim.cmd("qall")
						end
					end)
				end,
			})
		end,
	},
}
