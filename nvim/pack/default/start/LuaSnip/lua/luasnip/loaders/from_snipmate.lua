local ls = require("luasnip")
local cache = require("luasnip.loaders._caches").snipmate
local util = require("luasnip.util.util")
local loader_util = require("luasnip.loaders.util")
local Path = require("luasnip.util.path")
local session = require("luasnip.session")
local sp = require("luasnip.nodes.snippetProxy")

local function parse_snipmate(buffer, filename)
	local snippets = {}
	local extends = {}

	---@type string[]
	local lines = loader_util.split_lines(buffer)
	local i = 1

	local function _parse()
		local line = lines[i]
		local prefix, description = line:match([[^snippet%s+(%S+)%s*(.*)]])
		local body = {}

		i = i + 1
		---@type number
		local indent

		while i <= #lines do
			line = lines[i]
			if line:find("^%s+") then
				if not indent then
					indent = #line:match("^%s+")
				end
				line = line:sub(indent + 1)
				line = line:gsub("${VISUAL}", "${TM_SELECTED_TEXT}")
			elseif line ~= "" then
				break
			end
			table.insert(body, line)
			i = i + 1
		end

		body = table.concat(body, "\n")
		local snip = sp({
			trig = prefix,
			dscr = description,
			wordTrig = true,
		}, body)
		table.insert(snippets, snip)
	end

	while i <= #lines do
		local line = lines[i]
		if vim.startswith(line, "snippet") then
			_parse()
		elseif vim.startswith(line, "extends") then
			extends = vim.split(vim.trim(line:sub(8)), "[,%s]+")
			i = i + 1
		elseif vim.startswith(line, "#") or line:find("^%s*$") then
			-- comment and blank line
			i = i + 1
		else
			error(("invalid line in %s: %s"):format(filename, i))
		end
	end

	return snippets, extends
end

local function load_snippet_file(path)
	if not Path.exists(path) then
		return
	end

	local snippet, extends

	if cache.path_snippets[path] then
		snippet = cache.path_snippets[path].snippet
		extends = cache.path_snippets[path].extends
	else
		local buffer = Path.read_file(path)
		snippet, extends = parse_snipmate(buffer)
		cache.path_snippets[path] = { snippet = snippet, extends = extends }
	end

	return snippet, extends
end

local M = {}

function M._load(ft)
	local snippets = {}
	-- _load might be called for non-existing filetype via `extends`-directive,
	-- protect against that via `or {}` (we fail silently, though, maybe we
	-- should throw an error/print some message).
	for _, path in ipairs(cache.ft_paths[ft] or {}) do
		local snippet, extends = load_snippet_file(path)
		vim.list_extend(snippets, snippet)
		for _, extend in ipairs(extends) do
			vim.list_extend(snippets, M._load(extend))
		end
	end
	return snippets
end

function M.load(opts)
	opts = opts or {}

	if not opts.is_lazy then
		cache.ft_paths = loader_util.get_ft_paths(
			loader_util.normalize_paths(opts.paths, "snippets"),
			"snippets"
		)
	end

	local ft_filter = loader_util.ft_filter(opts.exclude, opts.include)

	for ft, _ in pairs(cache.ft_paths) do
		if ft_filter(ft) then
			local snippets = M._load(ft)
			ls.add_snippets(ft, snippets)
		end
	end
end

function M._lazyload()
	local fts = util.get_snippet_filetypes()
	for _, ft in ipairs(fts) do
		if not cache.lazy_loaded_ft[ft] then
			cache.lazy_loaded_ft[ft] = true
			M.load({ include = { ft }, is_lazy = true })
		end
	end
end

function M.lazy_load(opts)
	opts = opts or {}

	cache.ft_paths = loader_util.get_ft_paths(
		loader_util.normalize_paths(opts.paths, "snippets"),
		"snippets"
	)

	vim.cmd([[
    augroup _luasnip_snipmate_lazy_load
        au!
        au BufWinEnter,FileType * lua require("luasnip.loaders.from_snipmate")._lazyload()
        au User LuasnipCleanup lua require("luasnip.loaders._caches").snipmate:clean()
    augroup END
    ]])
end

return M
