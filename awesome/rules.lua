local awful = require("awful")

function add_rule (rule)
   table.insert(awful.rules.rules, rule)
end


add_rule({
      rule =  {
	 class = "jetbrains-studio",
	 name="^win[0-9]+$"
      },
      properties = {
	 titlebars_enabled = false,
      },
})

add_rule({
      rule_any =  {
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
      rule_any =  {
	 class = {
	    "Emacs",
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
      rule_any =  {
	 class = {
	    "discord",
	    "Evolution",
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
      rule_any =  {
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
}
