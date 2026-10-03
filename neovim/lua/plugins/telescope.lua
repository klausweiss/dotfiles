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
			{ "<leader>gB", "<cmd>Telescope lsp_implementations<cr>", desc = "LSP implementations" },
			-- Dynamic: re-queries the server as you type (most servers return nothing for an empty query)
			{ "<leader>gn", "<cmd>Telescope lsp_dynamic_workspace_symbols<cr>", desc = "LSP workspace symbols" },
		},
	},
}
