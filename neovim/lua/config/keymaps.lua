vim.keymap.set("n", "<leader>wv", "<cmd>vsplit<cr>", { desc = "Split window vertically" })
vim.keymap.set("n", "<leader>wh", "<cmd>split<cr>", { desc = "Split window horizontally" })

-- Asks to save `buf` if it has unsaved changes. Returns false if cancelled
local function confirm_save(buf)
	if not vim.bo[buf].modified then
		return true
	end
	local name = vim.fn.fnamemodify(vim.api.nvim_buf_get_name(buf), ":~:.")
	local choice = vim.fn.confirm(
		('Save changes to "%s"?'):format(name ~= "" and name or "[No Name]"),
		"&Yes\n&No\n&Cancel",
		1
	)
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

	-- Neither counts: the minimap closes itself once alone, and nvim-tree alone quits nvim
	local panes = vim.tbl_filter(function(w)
		local ft = vim.bo[vim.api.nvim_win_get_buf(w)].filetype
		return vim.api.nvim_win_get_config(w).relative == "" and ft ~= "minimap" and ft ~= "NvimTree"
	end, vim.api.nvim_tabpage_list_wins(0))
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
