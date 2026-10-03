-- LSP references, limited to the current file
local function file_references()
	local fname = vim.api.nvim_buf_get_name(0)
	vim.lsp.buf.references(nil, {
		on_list = function(list)
			local items = vim.tbl_filter(function(item)
				return item.filename == fname
			end, list.items)
			if #items == 0 then
				vim.notify("No references in this file", vim.log.levels.INFO)
				return
			end
			local conf = require("telescope.config").values
			require("telescope.pickers")
				.new({}, {
					prompt_title = "LSP References (file)",
					finder = require("telescope.finders").new_table({
						results = items,
						entry_maker = require("telescope.make_entry").gen_from_quickfix({ path_display = "hidden" }),
					}),
					sorter = conf.generic_sorter({}),
					previewer = conf.qflist_previewer({}),
				})
				:find()
		end,
	})
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
			{ "<leader>gd", "<cmd>Telescope lsp_definitions<cr>", desc = "LSP definition" },
			{ "<leader>gb", "<cmd>Telescope lsp_references<cr>", desc = "LSP references" },
			{ "<leader>gr", file_references, desc = "LSP references in file" },
			{ "<leader>gB", "<cmd>Telescope lsp_implementations<cr>", desc = "LSP implementations" },
			-- Dynamic: re-queries the server as you type (most servers return nothing for an empty query)
			{ "<leader>gn", "<cmd>Telescope lsp_dynamic_workspace_symbols<cr>", desc = "LSP workspace symbols" },
		},
	},
}
