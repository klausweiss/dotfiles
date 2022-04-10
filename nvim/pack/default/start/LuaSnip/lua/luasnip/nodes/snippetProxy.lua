-- the idea of this class is to lazily parse snippet (eg. only on expansion).
--
-- This is achieved by returning a proxy that has enough information to tell
-- whether the snippet should be expanded at a given point (eg. all fields
-- necessary to perform Snippet:matches()), but doesn't actually
-- have to parse the snippet, leaving up-front cost of loading a bunch of
-- snippets at a minimum.

local parse = require("luasnip.util.parser").parse_snippet
local snip_mod = require("luasnip.nodes.snippet")
local node_util = require("luasnip.nodes.util")

local SnippetProxy = {}

-- add Snippet-functions SnippetProxy can perform using the available data.
SnippetProxy.matches = snip_mod.Snippet.matches

function SnippetProxy:get_docstring()
	return self.docstring
end

function SnippetProxy:instantiate()
	-- self already contains initialized context and opts, can just be passed
	-- here, no problem.
	-- Bonus: if some keys are set on the snippets in the table (from the
	-- outside, for whatever reason), they are also present in the expanded
	-- snippet.
	--
	-- _S will copy self, so we can safely mutate (set metatables).
	local snippet = snip_mod._S(self, parse(nil, self._snippet_string))
	-- snippet will have snippetProxies `copy`, nil it in snippet so it calls
	-- snippet-copy via metatable.
	snippet.copy = nil

	self._snippet = snippet
	-- directly call into snippet on missing keys.
	setmetatable(self, {
		__index = self._snippet,
	})

	-- return snippet so it can provide a missing key.
	return snippet
end

-- context and opts are the same objects as in s(contex, nodes, opts), snippet is a string representing the snippet.
local function new(context, snippet, opts)
	-- "error": there should not be duplicate keys, don't silently overwrite/keep.
	local sp = vim.tbl_extend(
		"error",
		{},
		snip_mod.init_snippet_context(context),
		snip_mod.init_snippet_opts(opts),
		node_util.init_node_opts(opts)
	)

	sp._snippet_string = snippet
	-- override docstring
	sp.docstring = snippet

	setmetatable(sp, {
		__index = function(t, k)
			if SnippetProxy[k] then
				-- if it is possible to perform this operation without actually parsing the snippet, just do it.
				return SnippetProxy[k]
			else
				local snip = SnippetProxy.instantiate(t)
				if k == "_snippet" then
					return snip
				else
					return snip[k]
				end
			end
		end,
	})

	-- snippetProxy has to be able to return snippet on copy even after parsing,
	-- when the metatable has been changed. Therefore: set copy in each instance
	-- of snippetProxy.
	function sp:copy()
		return self._snippet:copy()
	end

	return sp
end

return new
