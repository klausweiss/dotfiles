local M = {}

-- Filetypes of side panels, which aren't editor panes
M.side_panels = { NvimTree = true, minimap = true }

-- Whether `win` is an editor pane: not a side panel or floating window
function M.is_editor_pane(win)
	return vim.api.nvim_win_get_config(win).relative == ""
		and not M.side_panels[vim.bo[vim.api.nvim_win_get_buf(win)].filetype]
end

-- Editor panes in a tab (default: the current one)
function M.editor_panes(tab)
	return vim.tbl_filter(M.is_editor_pane, vim.api.nvim_tabpage_list_wins(tab or 0))
end

return M
