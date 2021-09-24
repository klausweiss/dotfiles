local events = require("luasnip.util.events")
local session = require("luasnip.session")

local function get_cursor_0ind()
	local c = vim.api.nvim_win_get_cursor(0)
	c[1] = c[1] - 1
	return c
end

-- don't use utf-indexed column, win_set_cursor ignores these.
local function set_cursor_0ind(c)
	c[1] = c[1] + 1
	vim.api.nvim_win_set_cursor(0, c)
end

-- returns current line with text up-to and excluding the cursor.
local function get_current_line_to_cursor()
	local cur = get_cursor_0ind()
	-- cur-rows are 1-indexed, api-rows 0.
	local line = vim.api.nvim_buf_get_lines(0, cur[1], cur[1] + 1, false)
	return string.sub(line[1], 1, cur[2])
end

-- delete n chars before cursor, MOVES CURSOR
local function remove_n_before_cur(n)
	local cur = get_cursor_0ind()
	vim.api.nvim_buf_set_text(0, cur[1], cur[2] - n, cur[1], cur[2], { "" })
	cur[2] = cur[2] - n
	set_cursor_0ind(cur)
end

-- in-place modifies the table.
local function dedent(text, indentstring)
	-- 2 because 1 shouldn't contain indent.
	for i = 2, #text do
		text[i] = text[i]:gsub("^" .. indentstring, "")
	end
	return text
end

-- in-place insert indenstrig before each line.
local function indent(text, indentstring)
	for i = 2, #text - 1, 1 do
		-- only indent if there is actually text.
		if #text[i] > 0 then
			text[i] = indentstring .. text[i]
		end
	end
	-- assuming that the last line should be indented as it is probably
	-- followed by some other node, therefore isn't an empty line.
	if #text > 1 then
		text[#text] = indentstring .. text[#text]
	end
	return text
end

-- in-place expand tabs in leading whitespace.
local function expand_tabs(text, tabwidth)
	for i, line in ipairs(text) do
		local new_line = ""
		local start_indx = 1
		while true do
			local tab_indx = line:find("\t", start_indx, true)
			-- if no tab found, sub till end (ie. -1).
			new_line = new_line .. line:sub(start_indx, (tab_indx or 0) - 1)
			if tab_indx then
				-- #new_line is index of this tab in new_line.
				new_line = new_line
					.. string.rep(" ", tabwidth - #new_line % tabwidth)
			else
				-- reached end of string.
				break
			end
			start_indx = tab_indx + 1
		end
		text[i] = new_line
	end
	return text
end

local function tab_width()
	return vim.o.shiftwidth ~= 0 and vim.o.shiftwidth or vim.o.tabstop
end

local function mark_pos_equal(m1, m2)
	local p1 = vim.api.nvim_buf_get_extmark_by_id(0, Luasnip_ns_id, m1, {})
	local p2 = vim.api.nvim_buf_get_extmark_by_id(0, Luasnip_ns_id, m2, {})
	return p1[1] == p2[1] and p1[2] == p2[2]
end

local function move_to_mark(id)
	local new_cur_pos
	new_cur_pos = vim.api.nvim_buf_get_extmark_by_id(
		0,
		Luasnip_ns_id,
		id,
		{ details = false }
	)
	set_cursor_0ind(new_cur_pos)
end

local function bytecol_to_utfcol(pos)
	local line = vim.api.nvim_buf_get_lines(0, pos[1], pos[1] + 1, false)
	-- line[1]: get_lines returns table.
	return { pos[1], vim.str_utfindex(line[1] or "", pos[2]) }
end

local function normal_move_before(new_cur_pos)
	-- +1: indexing
	if new_cur_pos[2] - 1 ~= 0 then
		vim.api.nvim_feedkeys(
			tostring(new_cur_pos[1] + 1)
				.. "G0"
				.. tostring(new_cur_pos[2] - 1)
				-- open folds!
				.. "lzv",
			"n",
			true
		)
	else
		vim.api.nvim_feedkeys(tostring(new_cur_pos[1] + 1) .. "G0", "n", true)
	end
end

local function normal_move_on(new_cur_pos)
	if new_cur_pos[2] ~= 0 then
		vim.api.nvim_feedkeys(
			tostring(new_cur_pos[1] + 1)
				.. "G0"
				.. tostring(new_cur_pos[2])
				-- open folds!
				.. "lzv",
			"n",
			true
		)
	else
		vim.api.nvim_feedkeys(tostring(new_cur_pos[1] + 1) .. "G0", "n", true)
	end
end

local function normal_move_on_insert(new_cur_pos)
	local keys = vim.api.nvim_replace_termcodes(
		tostring(new_cur_pos[1] + 1)
			-- open folds!
			.. "G0zvi"
			.. string.rep("<Right>", new_cur_pos[2]),
		true,
		false,
		true
	)
	vim.api.nvim_feedkeys(keys, "n", true)
end

local function multiline_equal(t1, t2)
	for i, line in ipairs(t1) do
		if line ~= t2[i] then
			return false
		end
	end

	return #t1 == #t2
end

local function word_under_cursor(cur, line)
	local ind_start = 1
	local ind_end = #line

	while true do
		local tmp = string.find(line, "%W%w", ind_start)
		if not tmp then
			break
		end
		if tmp > cur[2] + 1 then
			break
		end
		ind_start = tmp + 1
	end

	local tmp = string.find(line, "%w%W", cur[2] + 1)
	if tmp then
		ind_end = tmp
	end

	return string.sub(line, ind_start, ind_end)
end

-- Put text and update cursor(pos) where cursor is byte-indexed.
local function put(text, pos)
	vim.api.nvim_buf_set_text(0, pos[1], pos[2], pos[1], pos[2], text)
	-- add rows
	pos[1] = pos[1] + #text - 1
	-- add columns, start at 0 if no rows were added, else at old col-value.
	pos[2] = (#text > 1 and 0 or pos[2]) + #text[#text]
end

-- Wrap a value in a table if it isn't one already
local function wrap_value(value)
	if not value or type(value) == "table" then
		return value
	end
	return { value }
end

-- Wrap node in a table if it is not one
local function wrap_nodes(nodes)
	-- safe to assume, if nodes has a metatable, it is a single node, not a
	-- table.
	if getmetatable(nodes) then
		return { nodes }
	else
		return nodes
	end
end

local SELECT_RAW = "LUASNIP_SELECT_RAW"
local SELECT_DEDENT = "LUASNIP_SELECT_DEDENT"
local TM_SELECT = "LUASNIP_TM_SELECT"

local function get_selection()
	local ok, val = pcall(vim.api.nvim_buf_get_var, 0, SELECT_RAW)
	if ok then
		local result = {
			val,
			vim.api.nvim_buf_get_var(0, SELECT_DEDENT),
			vim.api.nvim_buf_get_var(0, TM_SELECT),
		}

		vim.api.nvim_buf_del_var(0, SELECT_RAW)
		vim.api.nvim_buf_del_var(0, SELECT_DEDENT)
		vim.api.nvim_buf_del_var(0, TM_SELECT)

		return unpack(result)
	end
	return {}, {}, {}
end

local function get_min_indent(lines)
	-- "^(%s*)%S": match only lines that actually contain text.
	local min_indent = lines[1]:match("^(%s*)%S")
	for i = 2, #lines do
		-- %s* -> at least matches
		local line_indent = lines[i]:match("^(%s*)%S")
		-- ignore if not matched.
		if line_indent then
			-- if no line until now matched, use line_indent.
			if not min_indent or #line_indent < #min_indent then
				min_indent = line_indent
			end
		end
	end
	return min_indent
end

local function store_selection()
	local start_line, start_col = vim.fn.line("'<"), vim.fn.col("'<")
	local end_line, end_col = vim.fn.line("'>"), vim.fn.col("'>")
	local mode = vim.fn.visualmode()
	if
		not vim.o.selection == "exclusive"
		and not (start_line == end_line and start_col == end_col)
	then
		end_col = end_col - 1
	end

	local chunks = {}
	local lines = vim.api.nvim_buf_get_lines(0, start_line - 1, end_line, true)
	if start_line == end_line then
		chunks = { lines[1]:sub(start_col, end_col) }
	else
		local first_col = 0
		local last_col = nil
		if mode:lower() ~= "v" then -- mode is block
			first_col = start_col
			last_col = end_col
		end
		chunks = { lines[1]:sub(start_col, last_col) }

		-- potentially trim lines (Block).
		for cl = 2, #lines - 1 do
			table.insert(chunks, lines[cl]:sub(first_col, last_col))
		end
		table.insert(chunks, lines[#lines]:sub(first_col, end_col))
	end

	-- init with raw selection.
	local tm_select, select_dedent = vim.deepcopy(chunks), vim.deepcopy(chunks)
	-- may be nil if no indent.
	local min_indent = get_min_indent(lines) or ""
	-- TM_SELECTED_TEXT contains text from new cursor position(for V the first
	-- non-whitespace of first line, v and c-v raw) to end of selection.
	if mode == "V" then
		tm_select[1] = tm_select[1]:gsub("^%s+", "")
		-- remove indent from all lines:
		for i = 1, #select_dedent do
			select_dedent[i] = select_dedent[i]:gsub("^" .. min_indent, "")
		end
	elseif mode == "v" then
		-- if selection starts inside indent, remove indent.
		if #min_indent > start_col then
			select_dedent[1] = lines[1]:gsub(min_indent, "")
		end
		for i = 2, #select_dedent - 1 do
			select_dedent[i] = select_dedent[i]:gsub(min_indent, "")
		end

		-- remove as much indent from the last line as possible.
		if #min_indent > end_col then
			select_dedent[#select_dedent] = ""
		else
			select_dedent[#select_dedent] = select_dedent[#select_dedent]:gsub(
				"^" .. min_indent,
				""
			)
		end
	else
		-- in block: if indent is in block, remove the part of it that is inside
		-- it for select_dedent.
		if #min_indent > start_col then
			local indent_in_block = min_indent:sub(start_col, #min_indent)
			for i, line in ipairs(chunks) do
				select_dedent[i] = line:gsub("^" .. indent_in_block, "")
			end
		end
	end

	vim.api.nvim_buf_set_var(0, SELECT_RAW, chunks)
	vim.api.nvim_buf_set_var(0, SELECT_DEDENT, select_dedent)
	vim.api.nvim_buf_set_var(0, TM_SELECT, tm_select)
end

local function pos_equal(p1, p2)
	return p1[1] == p2[1] and p1[2] == p2[2]
end

local function clear_invalid(opts)
	for key, val in pairs(opts) do
		local act_group, pas_group, snip_pas_group =
			val.active.hl_group,
			val.passive.hl_group,
			val.snippet_passive.hl_group
		opts[key].snippet_passive.hl_group = vim.fn.hlexists(snip_pas_group)
					== 1
				and snip_pas_group
			or nil
		opts[key].passive.hl_group = vim.fn.hlexists(pas_group) == 1
				and pas_group
			or nil
		opts[key].active.hl_group = vim.fn.hlexists(act_group) == 1
				and act_group
			or nil
	end
end

local function make_opts_valid(user_opts, default_opts)
	local opts = vim.deepcopy(default_opts)
	for key, default_val in pairs(opts) do
		-- prevent nil-indexing error.
		user_opts[key] = user_opts[key] or {}

		-- override defaults with user for snippet_passive.
		default_val.snippet_passive = vim.tbl_extend(
			"force",
			default_val.snippet_passive,
			user_opts[key].snippet_passive or {}
		)
		-- override default-passive with user, get missing values from default
		-- snippet_passive
		default_val.passive = vim.tbl_extend(
			"force",
			user_opts[key].snippet_passive or {},
			vim.tbl_extend(
				"force",
				default_val.passive,
				user_opts[key].passive or {}
			)
		)
		-- same here, but with passive and active
		default_val.active = vim.tbl_extend(
			"force",
			default_val.passive,
			vim.tbl_extend(
				"force",
				default_val.active,
				user_opts[key].active or {}
			)
		)
	end
	return opts
end

local function increase_ext_prio(opts, amount)
	for _, val in pairs(opts) do
		val.active.priority = (val.active.priority or 0) + amount
		val.passive.priority = (val.passive.priority or 0) + amount
	end
	-- modifies in-place, but utilizing that may be cumbersome.
	return opts
end

local function string_wrap(lines, pos)
	local new_lines = vim.deepcopy(lines)
	if #new_lines == 1 and #new_lines[1] == 0 then
		return { "$" .. (pos and tostring(pos) or "{}") }
	end
	new_lines[1] = "${"
		.. (pos and (tostring(pos) .. ":") or "")
		.. new_lines[1]
	new_lines[#new_lines] = new_lines[#new_lines] .. "}"
	return new_lines
end

-- Heuristic to extract the comment style from the commentstring
local _comments_cache = {}
local function buffer_comment_chars()
	local commentstring = vim.bo.commentstring
	if _comments_cache[commentstring] then
		return _comments_cache[commentstring]
	end
	local comments = { "//", "/*", "*/" }
	local placeholder = "%s"
	local index_placeholder = commentstring:find(vim.pesc(placeholder))
	if index_placeholder then
		index_placeholder = index_placeholder - 1
		if index_placeholder + #placeholder == #commentstring then
			comments[1] = vim.trim(commentstring:sub(1, -#placeholder - 1))
		else
			comments[2] = vim.trim(commentstring:sub(1, index_placeholder))
			comments[3] = vim.trim(
				commentstring:sub(index_placeholder + #placeholder + 1, -1)
			)
		end
	end
	_comments_cache[commentstring] = comments
	return comments
end

local function to_line_table(table_or_string)
	local tbl = wrap_value(table_or_string)

	-- split entries at \n.
	local line_table = {}
	for _, str in ipairs(tbl) do
		local split = vim.split(str, "\n", true)
		for i = 1, #split do
			line_table[#line_table + 1] = split[i]
		end
	end

	return line_table
end

local function find_outer_snippet(node)
	while node.parent do
		node = node.parent
	end
	return node
end

-- filetype: string formatted like `'filetype'`.
local function get_snippet_filetypes(filetype)
	local fts = vim.split(filetype, ".", true)
	local snippet_fts = {}
	for _, ft in ipairs(fts) do
		vim.list_extend(snippet_fts, session.ft_redirect[ft])
	end
	-- add all last.
	table.insert(snippet_fts, "all")
	return snippet_fts
end

return {
	get_cursor_0ind = get_cursor_0ind,
	set_cursor_0ind = set_cursor_0ind,
	get_ext_positions = get_ext_positions,
	get_ext_position_begin = get_ext_position_begin,
	get_ext_position_end = get_ext_position_end,
	move_to_mark = move_to_mark,
	normal_move_before = normal_move_before,
	normal_move_on = normal_move_on,
	normal_move_on_insert = normal_move_on_insert,
	remove_n_before_cur = remove_n_before_cur,
	get_current_line_to_cursor = get_current_line_to_cursor,
	mark_pos_equal = mark_pos_equal,
	multiline_equal = multiline_equal,
	word_under_cursor = word_under_cursor,
	put = put,
	wrap_value = wrap_value,
	wrap_nodes = wrap_nodes,
	store_selection = store_selection,
	get_selection = get_selection,
	pos_equal = pos_equal,
	dedent = dedent,
	indent = indent,
	expand_tabs = expand_tabs,
	tab_width = tab_width,
	make_opts_valid = make_opts_valid,
	increase_ext_prio = increase_ext_prio,
	clear_invalid = clear_invalid,
	buffer_comment_chars = buffer_comment_chars,
	string_wrap = string_wrap,
	to_line_table = to_line_table,
	find_outer_snippet = find_outer_snippet,
	get_snippet_filetypes = get_snippet_filetypes,
}
