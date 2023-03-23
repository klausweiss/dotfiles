local awful = require("awful")

local screens = require("lib/screens")

function add_rule(rule)
	table.insert(awful.rules.rules, rule)
end

SCREEN_LEFT = "DP-1-3"
SCREEN_MID = "DP-1-2"
SCREEN_RIGHT = "eDP-1"

add_rule({
	rule = {
		class = "jetbrains-studio",
		name = "^win[0-9]+$"
	},
	properties = {
		titlebars_enabled = false,
	},
})

add_rule({
	rule_any = {
		class = {
			"Arandr",
		},
	},
	properties = {
		floating = true,
	},
})

-- dev tag
add_rule({
	rule_any = {
		class = {
			"Emacs",
			"Codium",
			"jetbrains-idea-ce",
			"jetbrains-pycharm",
			"jetbrains-pycharm-ce",
			"jetbrains-studio",
		},
	},
	properties = {
		tag = "2",
	},
})
-- communication tag
add_rule({
	rule_any = {
		class = {
			"discord",
			"Evolution",
			"geary",
			"rambox",
			"Signal",
			"Telegram",
			"Thunderbird",
		},
	},
	properties = {
		tag = "4",
	},
})
-- web tag
add_rule({
	rule_any = {
		class = {
			"Firefox",
		},
	},
	properties = {
		tag = "1",
	},
})


return {
	add_rule = add_rule,
	get_left_screen = screens.get_screen_by_name(SCREEN_LEFT),
	get_mid_screen = screens.get_screen_by_name(SCREEN_MID),
	get_right_screen = screens.get_screen_by_name(SCREEN_RIGHT),
}
