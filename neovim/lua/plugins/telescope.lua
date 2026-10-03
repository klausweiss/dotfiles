-- Flattens buf_request_all responses into a list of Location / LocationLink
local function locations(responses)
	local all = {}
	for _, response in pairs(responses) do
		local result = response.result
		if result then
			vim.list_extend(all, result.uri and { result } or result)
		end
	end
	return all
end

-- Whether a Location / LocationLink points at the cursor position in `params`
local function at_cursor(loc, params)
	local uri = loc.targetUri or loc.uri
	local range = loc.targetSelectionRange or loc.range
	local pos, s, e = params.position, range.start, range["end"]
	return uri == params.textDocument.uri
		and (pos.line > s.line or (pos.line == s.line and pos.character >= s.character))
		and (pos.line < e.line or (pos.line == e.line and pos.character <= e.character))
end

-- IntelliJ-style navigation: on a usage, jump to its definition. On the
-- definition itself, list implementations, or usages if there are none.
local function definition_or_usages()
	local builtin = require("telescope.builtin")
	local params = vim.lsp.util.make_position_params(0, "utf-16")
	vim.lsp.buf_request_all(0, "textDocument/definition", params, function(responses)
		local on_definition = vim.iter(locations(responses)):any(function(loc)
			return at_cursor(loc, params)
		end)
		if not on_definition then
			builtin.lsp_definitions()
			return
		end
		vim.lsp.buf_request_all(0, "textDocument/implementation", params, function(impl_responses)
			-- Some servers (e.g. basedpyright) report a concrete method as its own implementation
			local others = vim.iter(locations(impl_responses)):any(function(loc)
				return not at_cursor(loc, params)
			end)
			if others then
				builtin.lsp_implementations()
			else
				builtin.lsp_references({ include_declaration = false })
			end
		end)
	end)
end

return {
	{
		"nvim-telescope/telescope.nvim",
		version = "*",
		dependencies = {
			"nvim-lua/plenary.nvim",
			-- optional but recommended
			{ "nvim-telescope/telescope-fzf-native.nvim", build = "make" },
		},
		keys = {
			{ "<leader>p", "<cmd>Telescope commands<cr>", desc = "Command palette" },
			{ "<leader>fo", "<cmd>Telescope find_files<cr>", desc = "Find files" },
			{ "<leader>gb", definition_or_usages, desc = "LSP definition, or implementations / usages" },
			-- Dynamic: re-queries the server as you type (most servers return nothing for an empty query)
			{ "<leader>gn", "<cmd>Telescope lsp_dynamic_workspace_symbols<cr>", desc = "LSP workspace symbols" },
		},
	},
}
