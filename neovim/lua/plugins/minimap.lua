-- The plugin renders via the external `code-minimap` binary. If it isn't
-- installed, it's built with nix into this out-link (also a GC root)
local nix_minimap = vim.fs.joinpath(vim.fn.stdpath("data"), "code-minimap")
local nixpkgs = "github:NixOS/nixpkgs/d793f675fa0275b67fe3271c7f612182cf190f63"

return {
	{
		"wfxr/minimap.vim",
		lazy = false,
		build = function()
			if vim.fn.executable("code-minimap") == 1 then
				return
			end
			local out = vim.fn.system({ "nix", "build", "--out-link", nix_minimap, nixpkgs .. "#code-minimap" })
			if vim.v.shell_error ~= 0 then
				error("nix build of code-minimap failed:\n" .. out)
			end
		end,
		keys = {
			{ "<leader>mm", "<cmd>MinimapToggle<cr>", desc = "Toggle minimap" },
		},
		init = function()
			if vim.fn.executable("code-minimap") == 0 then
				vim.env.PATH = vim.fs.joinpath(nix_minimap, "bin") .. ":" .. vim.env.PATH
			end
			-- Instead of minimap_auto_start, which opens it on VimEnter even for a bare
			-- `nvim` (the extra window clears the intro screen): open it once, with the
			-- first real file. After that it's up to <leader>mm
			vim.api.nvim_create_autocmd("BufWinEnter", {
				callback = function(args)
					if vim.bo[args.buf].buftype ~= "" or args.file == "" then
						return
					end
					-- Deferred so the window layout is settled (e.g. during startup)
					vim.schedule(function()
						vim.cmd("Minimap")
					end)
					return true -- remove this autocmd
				end,
			})
			vim.g.minimap_git_colors = 1
			vim.g.minimap_highlight_search = 1
		end,
	},
}
