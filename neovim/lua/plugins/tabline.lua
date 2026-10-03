local theme = {
	fill = "TabLineFill",
	head = "TabLine",
	current_tab = "TabLineSel",
	tab = "TabLine",
	win = "TabLine",
	tail = "TabLine",
}

-- The pane a tab is "about": its focused pane, or its first editor pane when a
-- side panel is focused
local function tab_main_win(tabid)
	local panes = require("config.panes")
	local win = vim.api.nvim_tabpage_get_win(tabid)
	if not panes.is_editor_pane(win) then
		win = panes.editor_panes(tabid)[1] or win
	end
	return win
end

local function win_file_name(win)
	return vim.fn.fnamemodify(vim.api.nvim_buf_get_name(vim.api.nvim_win_get_buf(win)), ":t")
end

return {
	{
		"nanozuki/tabby.nvim",
		dependencies = { "nvim-tree/nvim-web-devicons" },
		init = function()
			-- Only show the tabline when there are at least two tabs
			vim.o.showtabline = 1
		end,
		config = function()
			require("tabby").setup({
				-- Tabs on the left, the current tab's editor panes on the right
				-- (based on the README example, minus side panels)
				line = function(line)
					return {
						{
							{ "  ", hl = theme.head },
							line.sep("", theme.head, theme.fill),
						},
						line.tabs().foreach(function(tab)
							local hl = tab.is_current() and theme.current_tab or theme.tab
							return {
								line.sep("", hl, theme.fill),
								(require("nvim-web-devicons").get_icon(
									win_file_name(tab_main_win(tab.id)),
									nil,
									{ default = true }
								)),
								tab.number(),
								tab.name(),
								tab.close_btn(""),
								line.sep("", hl, theme.fill),
								hl = hl,
								margin = " ",
							}
						end),
						line.spacer(),
						line.wins_in_tab(line.api.get_current_tab())
							.filter(function(win)
								return require("config.panes").is_editor_pane(win.id)
							end)
							.foreach(function(win)
								return {
									line.sep("", theme.win, theme.fill),
									win.is_current() and "" or "",
									win.file_icon(),
									win.buf_name(),
									line.sep("", theme.win, theme.fill),
									hl = theme.win,
									margin = " ",
								}
							end),
						{
							line.sep("", theme.tail, theme.fill),
							{ "  ", hl = theme.tail },
						},
						hl = theme.fill,
					}
				end,
				option = {
					buf_name = { mode = "unique" },
					-- Unnamed tabs: the focused file, plus how many other editor panes there are
					tab_name = {
						name_fallback = function(tabid)
							local editor_panes = require("config.panes").editor_panes(tabid)
							local name = win_file_name(tab_main_win(tabid))
							name = name ~= "" and name or "[No Name]"
							return #editor_panes > 1 and ("%s [%d+]"):format(name, #editor_panes - 1) or name
						end,
					},
				},
			})
		end,
	},
}
