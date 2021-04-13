local awful = require("awful")
local beautiful = require("beautiful")
local gears = require("gears")
local menubar = require("menubar")
local wibox = require("wibox")
local dpi = require("beautiful.xresources").apply_dpi

local rules = require("rules")

-- {{{ Signals
-- Signal function to execute when a new client appears.
function place_client(c)
   if awesome.startup and
      not c.size_hints.user_position and
      not c.size_hints.program_position
   then
      awful.placement.no_offscreen(c)
   end
end

-- https://www.reddit.com/r/awesomewm/comments/bva8t2/icon_theme_support/
function swap_icon_for_gtk(c)
   local icon_candidates = {
      c.class,
      c.instance,
      c.class and c.class:lower(),
      c.instance and c.instance:lower(),
   }

   for _, icon_name in ipairs(icon_candidates) do
      local icon = menubar.utils.lookup_icon(icon_name)
      if icon ~= nil then
	 c.icon = gears.surface(icon)._native
	 return
      end
   end
   c.icon = gears.surface(menubar.utils.lookup_icon("application-default-icon"))._native
end

client.connect_signal(
   "manage", function (c)
      place_client(c)
      swap_icon_for_gtk(c)
end)

-- autofocus
require("awful.autofocus")
local handler_mouse_enter = function (client_)
   if awful.layout.get(client_.screen) ~= awful.layout.suit.magnifier
      and awful.client.focus.filter(client_) then
      client.focus = client_
   end
end
client.connect_signal("mouse::enter", handler_mouse_enter)

local floatingontopbutton = function(c)
   local fbut = awful.titlebar.widget.floatingbutton(c)
   local otbut = awful.titlebar.widget.ontopbutton(c)
   fbut:buttons(gears.table.join(
		   otbut:buttons(),
		   fbut:buttons()
   ))
   c.ontop = c.floating
   c:connect_signal("property::floating_geometry", function()
		       c.ontop = true
   end)
   return fbut
end


-- Add a titlebar if titlebars_enabled is set to true in the rules.
local handler_request_titlebars = function(c)
   -- buttons for the titlebar
   local buttons = gears.table.join(
      awful.button({ }, 1, function()
	    c:emit_signal("request::activate", "titlebar", {raise = true})
	    awful.mouse.client.move(c)
      end),
      awful.button({ }, 3, function()
	    c:emit_signal("request::activate", "titlebar", {raise = true})
	    awful.mouse.client.resize(c)
      end)
   )

   local titlebar_args = {
      position = "right"
   }
   awful.titlebar(c, titlebar_args) : setup
   {
      { -- Left
	 awful.titlebar.widget.closebutton    (c),
	 awful.titlebar.widget.minimizebutton (c),
	 awful.titlebar.widget.maximizedbutton(c),
	 layout  = wibox.layout.fixed.vertical,
      },
      { -- Middle
	 buttons = buttons,
	 layout  = wibox.layout.flex.vertical,
      },
      { -- Right
	 floatingontopbutton (c),
	 awful.titlebar.widget.stickybutton   (c),
	 layout = wibox.layout.fixed.vertical,
      },
      layout = wibox.layout.align.vertical,
   }
end

client.connect_signal("request::titlebars", handler_request_titlebars)

-- Enable sloppy focus, so that focus follows mouse.
client.connect_signal(
   "mouse::enter", function(c)
      c:emit_signal("request::activate", "mouse_enter", {raise = false})
end)

client.connect_signal("focus", function(c) c.border_color = beautiful.border_focus end)
client.connect_signal("unfocus", function(c) c.border_color = beautiful.border_normal end)
-- }}}

rules.add_rule({
      rule_any = {
	 type = { "normal", "dialog" }
      },
      properties = {
	 titlebars_enabled = true,
	 size_hints_honor = false,
      },
})

rules.add_rule({
      rule = {},
      properties = {
	 screen = awful.screen.preferred,
	 placement = awful.placement.no_overlap + awful.placement.no_offscreen,
	 focus = awful.client.focus.filter,
      },
})
