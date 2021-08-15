return {
	-- supported lsp-vars.
	lsp = {
		TM_CURRENT_LINE = true,
		TM_CURRENT_WORD = true,
		TM_LINE_INDEX = true,
		TM_LINE_NUMBER = true,
		TM_FILENAME = true,
		TM_FILENAME_BASE = true,
		TM_DIRECTORY = true,
		TM_FILEPATH = true,
		TM_SELECTED_TEXT = true,
		SELECT_RAW = true,
		SELECT_DEDENT = true,
		var = function(_, node, text)
			local v = node.parent.env[text]
			if type(v) == "table" then
				return v
			else
				return { v }
			end
		end,
	},
	copy = function(args)
		return args[1]
	end,
}
