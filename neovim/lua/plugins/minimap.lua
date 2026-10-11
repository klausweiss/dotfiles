-- The plugin renders via the external `code-minimap` binary
return {
	{
		"wfxr/minimap.vim",
		lazy = false,
		build = require("config.nix").bin("code-minimap"),
		keys = {
			{ "<leader>mm", "<cmd>MinimapToggle<cr>", desc = "Toggle minimap" },
		},
		init = function()
			-- Instead of minimap_auto_start, which opens it on VimEnter even for a bare
			-- `nvim` (the extra window clears the intro screen): open it once, with the
			-- first real file. After that it's up to <leader>mm
			vim.api.nvim_create_autocmd("BufWinEnter", {
				callback = function(args)
					if vim.bo[args.buf].buftype ~= "" or args.file == "" then
						return
					end
					-- Deferred so the window layout is settled (e.g. during startup)
					vim.schedule(function()
						vim.cmd("Minimap")
					end)
					return true -- remove this autocmd
				end,
			})
			vim.g.minimap_git_colors = 1
			vim.g.minimap_highlight_search = 1
		end,
	},
}
