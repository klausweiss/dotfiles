local M = {}

M.nixpkgs = "github:NixOS/nixpkgs/d793f675fa0275b67fe3271c7f612182cf190f63"

-- Provides external binaries from nixpkgs, unless they're already installed:
-- puts their nix out-links (also GC roots) on PATH and returns a lazy.nvim
-- `build` function that builds them there.
-- Each argument is a binary name, or { name, pkg = nixpkgs attribute (default:
-- name), unfree = allow the package (or its dependencies) to be unfree }
--   build = require("config.nix").bin("code-minimap", { "foo", pkg = "foo-cli" })
function M.bin(...)
	local missing = {}
	for _, spec in ipairs({ ... }) do
		spec = type(spec) == "string" and { spec } or spec
		if vim.fn.executable(spec[1]) == 0 then
			spec.out_link = vim.fs.joinpath(vim.fn.stdpath("data"), "nix", spec[1])
			vim.env.PATH = vim.fs.joinpath(spec.out_link, "bin") .. ":" .. vim.env.PATH
			table.insert(missing, spec)
		end
	end
	if #missing == 0 then
		return nil
	end
	return function()
		for _, spec in ipairs(missing) do
			local cmd = { "nix", "build", "--out-link", spec.out_link, M.nixpkgs .. "#" .. (spec.pkg or spec[1]) }
			if spec.unfree then
				-- Flakes only honour NIXPKGS_ALLOW_UNFREE with --impure
				table.insert(cmd, 3, "--impure")
			end
			local res = vim.system(cmd, { env = spec.unfree and { NIXPKGS_ALLOW_UNFREE = "1" } or nil }):wait()
			if res.code ~= 0 then
				error(("nix build of %s failed:\n%s"):format(spec[1], res.stderr))
			end
		end
	end
end

return M
