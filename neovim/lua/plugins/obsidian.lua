-- Dynamic workspace: <project>/wiki/src, found by walking up from the current
-- buffer (or the cwd if it has no name). Returns nil if there is none.
local function find_wiki()
	local buf_dir = vim.fs.dirname(vim.api.nvim_buf_get_name(0))
	local start = (buf_dir and buf_dir ~= "") and buf_dir or vim.uv.cwd()
	for dir in vim.fs.parents(start .. "/") do
		local wiki = vim.fs.joinpath(dir, "wiki", "src")
		if vim.fn.isdirectory(wiki) == 1 then
			return wiki
		end
	end
end

return {
	{
		"obsidian-nvim/obsidian.nvim",
		version = "*", -- use latest release, remove to use latest commit
		-- Workspaces are resolved once at startup and at least one is required, so
		-- only load the plugin when a wiki exists (otherwise e.g. /tmp would become
		-- the vault and get scanned for backlinks).
		cond = function()
			return find_wiki() ~= nil
		end,
		-- obsidian.nvim's UI features need conceallevel 1 or 2
		init = function()
			vim.api.nvim_create_autocmd("FileType", {
				pattern = "markdown",
				callback = function()
					vim.opt_local.conceallevel = 1
				end,
			})
		end,
		---@module 'obsidian'
		---@type obsidian.config
		opts = {
			legacy_commands = false, -- this will be removed in 4.0.0
			workspaces = {
				{
					name = "project-wiki",
					path = find_wiki,
					overrides = {
						-- daily/2026-09/2026-09-21.md
						daily_notes = {
							folder = "daily",
							date_format = "YYYY-MM/YYYY-MM-DD",
						},
					},
				},
			},
		},
	},
}
