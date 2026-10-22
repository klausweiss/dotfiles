-- The plugin renders via the external `code-minimap` binary
return {
	{
		"wfxr/minimap.vim",
		lazy = false,
		build = require("config.nix").bin("code-minimap"),
		keys = {
			{ "<F2>", "<cmd>MinimapToggle<cr>", mode = { "n", "i" }, desc = "Toggle minimap" },
		},
		init = function()
			-- Instead of minimap_auto_start, which opens it on VimEnter even for a bare
			-- `nvim` (the extra window clears the intro screen): open it once, with the
			-- first real file. After that it's up to <F2>
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

			-- Scrolling a focused minimap moves its cursor a line (buffer-local maps to
			-- j/k), which moves the file along. Unfocused, those maps don't apply and
			-- scrolling only shifts the minimap's view, out of sync with the file. So
			-- do the same step from here: from the file's spot in the minimap, a line
			-- up/down, back to a file line (the plugin's math, see s:minimap_move)
			local function step(mmwin, delta)
				local win = vim.api.nvim_get_current_win()
				if not require("config.panes").is_editor_pane(win) then
					return
				end
				local mm_height = vim.fn.line("w$", mmwin)
				local height = vim.api.nvim_buf_line_count(0)
				local line = vim.api.nvim_win_get_cursor(win)[1]
				local pos = vim.fn["minimap#vim#BufferToMap"](line - 1, height, mm_height)
				pos = math.min(math.max(pos + delta, 1), mm_height)
				line = math.floor((pos - 1) / mm_height * height) + 2
				vim.api.nvim_win_set_cursor(win, { math.min(line, height), 0 })
			end

			local function over_unfocused_minimap()
				local win = vim.fn.getmousepos().winid
				return win ~= 0
					and win ~= vim.api.nvim_get_current_win()
					and vim.bo[vim.api.nvim_win_get_buf(win)].filetype == "minimap"
					and win
			end

			for _, mod in ipairs({ "", "2-", "3-", "4-", "S-", "C-" }) do
				for _, dir in ipairs({ "Up", "Down", "Left", "Right" }) do
					local key = "<" .. mod .. "ScrollWheel" .. dir .. ">"
					-- Like a focused minimap: plain up/down navigate, the rest do nothing
					local delta = (mod == "" or mod:match("^%d")) and ({ Up = -1, Down = 1 })[dir]
					vim.keymap.set({ "n", "v", "i" }, key, function()
						local mmwin = over_unfocused_minimap()
						if not mmwin then
							return key
						end
						if delta then
							-- Expr mappings can't move the cursor
							vim.schedule(function()
								step(mmwin, delta)
							end)
						end
						return ""
					end, { expr = true })
				end
			end
		end,
	},
}
