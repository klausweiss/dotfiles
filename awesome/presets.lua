local awful = require("awful")

local rules = require("rules")


local function _match_any_classname(names)
	return function(c)
		return awful.rules.match_any(c, { class = names })
	end
end

local function _match_any_windowname(names)
	return function(c)
		return awful.rules.match_any(c, { name = names })
	end
end

local pycharm = _match_any_classname({
	"jetbrains-idea-ce",
	"jetbrains-pycharm",
	"jetbrains-pycharm-ce",
	"jetbrains-studio",
})
local emacs = _match_any_classname({ "Emacs" })
local slack = _match_any_classname({ "Slack" })
local zoom = _match_any_classname({ "zoom" })
local browser = _match_any_classname({ "Google-chrome", "firefox" })
local terminal = _match_any_classname({ "kitty" })
local music = _match_any_windowname({ "Programy Player Polskie Radio SA - Google Chrome" })

local left = rules.get_left_screen
local middle = rules.get_mid_screen
local right = rules.get_right_screen

local function preset(preset_rules)
	local function reversedipairsiter(t, i)
		i = i - 1
		if i ~= 0 then
			return i, t[i]
		end
	end
	local function reversedipairs(t)
		return reversedipairsiter, t, #t + 1
	end

	return function()
		for i, rule in reversedipairs(preset_rules) do
			local match = rule[1]
			local what_to_do = rule[2]
			for c in awful.client.iterate(match) do
				local new_t = what_to_do["tag"]
				local new_s = what_to_do["screen"]()
				local should_focus = true
				if what_to_do["focus"] ~= nil then
					should_focus = what_to_do["focus"]
				end

				for _, t in pairs(new_s.tags) do
					if t.name == new_t then
						c:move_to_tag(t) -- also moves to correct screen
						if should_focus then
							t:view_only()
							awful.screen.focus(new_s)
						end
						break
					end
				end
			end
		end
	end
end

local preset_coding = preset({
	{ pycharm,  { tag = "2", screen = middle } },
	{ music,    { tag = "5", screen = left, focus = false } },
	{ terminal, { tag = "3", screen = left } },
	{ browser,  { tag = "1", screen = middle } },
	{ emacs,    { tag = "2", screen = left } },
	{ slack,    { tag = "4", screen = right } },
	{ zoom,     { tag = "4", screen = right } },
})

local preset_web = preset({
	{ music,   { tag = "5", screen = left, focus = false } },
	{ browser, { tag = "1", screen = middle } },
	{ emacs,   { tag = "2", screen = left } },
	{ slack,   { tag = "4", screen = right } },
	{ zoom,    { tag = "4", screen = right } },
})

local preset_meeting = preset({
	{ zoom,    { tag = "4", screen = middle } },
	{ music,   { tag = "5", screen = left, focus = false } },
	{ browser, { tag = "1", screen = left } },
	{ slack,   { tag = "4", screen = right } },
})

return {
	preset_coding = preset_coding,
	preset_meeting = preset_meeting,
	preset_web = preset_web,
}
