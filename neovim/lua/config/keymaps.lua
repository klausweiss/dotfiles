-- Filetypes of side panels, which aren't editor panes
local side_panels = { NvimTree = true, minimap = true }

-- Editor panes in the current tab: no side panels or floating windows
local function editor_panes()
	return vim.tbl_filter(function(w)
		return vim.api.nvim_win_get_config(w).relative == ""
			and not side_panels[vim.bo[vim.api.nvim_win_get_buf(w)].filetype]
	end, vim.api.nvim_tabpage_list_wins(0))
end

-- Zoomed pane (see <leader>wz): { win = floating window, origin = zoomed pane }
local zoom

vim.keymap.set("n", "<leader>wv", "<cmd>vsplit<cr>", { desc = "Split window vertically" })
vim.keymap.set("n", "<leader>wh", "<cmd>split<cr>", { desc = "Split window horizontally" })

-- Asks to save `buf` if it has unsaved changes. Returns false if cancelled
local function confirm_save(buf)
	if not vim.bo[buf].modified then
		return true
	end
	local name = vim.fn.fnamemodify(vim.api.nvim_buf_get_name(buf), ":~:.")
	local choice =
		vim.fn.confirm(('Save changes to "%s"?'):format(name ~= "" and name or "[No Name]"), "&Yes\n&No\n&Cancel", 1)
	if choice == 1 then
		-- confirm: asks for a file name if the buffer doesn't have one
		vim.cmd("confirm write")
		return not vim.bo[buf].modified
	end
	return choice == 2
end

-- Close the current pane. If no other pane shows its file, close the file too
-- (asking to save unsaved changes). The last editor pane stays open with
-- another file (or an empty buffer) in it.
local function close_pane()
	local win = vim.api.nvim_get_current_win()
	local buf = vim.api.nvim_win_get_buf(win)
	-- Unlisted buffers (nvim-tree, help, ...) aren't files to close
	local close_file = vim.bo[buf].buflisted and #vim.fn.win_findbuf(buf) == 1
	if close_file and not confirm_save(buf) then
		return
	end

	-- Side panels don't count: the minimap closes itself once alone, and nvim-tree alone quits nvim
	local panes = editor_panes()
	if #panes > 1 or not vim.list_contains(panes, win) then
		vim.api.nvim_win_close(win, true)
	else
		local alt = vim.fn.bufnr("#")
		if alt ~= -1 and alt ~= buf and vim.bo[alt].buflisted then
			vim.cmd.buffer(alt)
		else
			vim.cmd.enew()
		end
	end

	if close_file and vim.api.nvim_buf_is_valid(buf) then
		vim.api.nvim_buf_delete(buf, { force = true })
	end
end
vim.keymap.set("n", "<leader>wc", close_pane, { desc = "Close pane" })

-- Cycle through editor panes, like tmux's `prefix o`
vim.keymap.set("n", "<leader>wo", function()
	local panes = editor_panes()
	if #panes == 0 then
		return
	end
	-- When zoomed, continue from the zoomed pane (moving to it unzooms, like tmux).
	-- From a side panel, index() is -1, so this goes to the first pane
	local current = zoom and zoom.origin or vim.api.nvim_get_current_win()
	local i = vim.fn.index(panes, current) + 1
	vim.api.nvim_set_current_win(panes[i % #panes + 1])
end, { desc = "Next pane" })

-- Zoom the current pane, like tmux's `prefix z`: show it in a floating window
-- covering the editor, leaving the layout underneath untouched. Unzooming
-- carries the cursor position back to the original pane
local function unzoom()
	local z = zoom
	zoom = nil
	if not z or not vim.api.nvim_win_is_valid(z.win) then
		return
	end
	local buf = vim.api.nvim_win_get_buf(z.win)
	local view = vim.api.nvim_win_call(z.win, vim.fn.winsaveview)
	local was_current = vim.api.nvim_get_current_win() == z.win
	vim.api.nvim_win_close(z.win, false)
	if vim.api.nvim_win_is_valid(z.origin) then
		if vim.api.nvim_win_get_buf(z.origin) == buf then
			vim.api.nvim_win_call(z.origin, function()
				vim.fn.winrestview(view)
			end)
		end
		if was_current then
			vim.api.nvim_set_current_win(z.origin)
		end
	end
end

local function zoom_size()
	local tabline = vim.o.showtabline == 2 or (vim.o.showtabline == 1 and #vim.api.nvim_list_tabpages() > 1)
	local row = tabline and 1 or 0
	return { row = row, col = 0, width = vim.o.columns, height = vim.o.lines - vim.o.cmdheight - row }
end

vim.keymap.set("n", "<leader>wz", function()
	if zoom then
		unzoom()
		return
	end
	local origin = vim.api.nvim_get_current_win()
	if not vim.list_contains(editor_panes(), origin) then
		return -- side panel or floating window
	end
	local view = vim.fn.winsaveview()
	-- Not "minimal": the float inherits the pane's options (line numbers etc.)
	local win = vim.api.nvim_open_win(0, true, vim.tbl_extend("force", zoom_size(), { relative = "editor" }))
	vim.wo[win].winhighlight = "NormalFloat:Normal"
	vim.fn.winrestview(view)
	zoom = { win = win, origin = origin }
end, { desc = "Toggle pane zoom" })

vim.api.nvim_create_autocmd("WinEnter", {
	desc = "Unzoom when moving to another pane (popups like Telescope are floats, so they don't count)",
	callback = function()
		if zoom and vim.api.nvim_win_get_config(0).relative == "" then
			unzoom()
		end
	end,
})
vim.api.nvim_create_autocmd("WinClosed", {
	callback = function(args)
		if zoom and tonumber(args.match) == zoom.win then
			zoom = nil
		end
	end,
})
vim.api.nvim_create_autocmd("VimResized", {
	callback = function()
		if zoom and vim.api.nvim_win_is_valid(zoom.win) then
			vim.api.nvim_win_set_config(zoom.win, vim.tbl_extend("force", zoom_size(), { relative = "editor" }))
		end
	end,
})
