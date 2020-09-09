local awful = require("awful")
local gears = require("gears")

local keyboard = require("keyboard")
local rules = require("rules")


root.buttons(gears.table.join(
		awful.button({ }, 3, function () mymainmenu:toggle() end),
		awful.button({ }, 4, awful.tag.viewprev),
		awful.button({ }, 5, awful.tag.viewnext)
))


clientbuttons = gears.table.join(
   awful.button({ }, 1, function (c)
	 c:emit_signal("request::activate", "mouse_click", {raise = true})
   end),
   awful.button({ keyboard.clients_modkey }, 1, function (c)
	 c:emit_signal("request::activate", "mouse_click", {raise = true})
	 awful.mouse.client.move(c)
   end),
   awful.button({ keyboard.clients_modkey }, 3, function (c)
	 c:emit_signal("request::activate", "mouse_click", {raise = true})
	 awful.mouse.client.resize(c)
   end)
)

rules.add_rule(
   { rule = { },
     properties = {
	buttons = clientbuttons,
     },
   }
)
