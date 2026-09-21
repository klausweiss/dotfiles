return {
	{
		"jakewvincent/mkdnflow.nvim",
		ft = { "markdown", "rmd" }, -- Add custom filetypes here if configured
		config = function()
			require("mkdnflow").setup({
				mappings = {
					MkdnFoldSection = false,
				},
			})
		end,
	},
}
