local awful = require("awful")
local wibox = require("wibox")
local dpi = require("beautiful.xresources").apply_dpi

local startmenu = require("startmenu")
local battery = require("lib/widgets/battery")
local sound = require("lib/widgets/sound")
local taglist = require("lib/widgets/taglist")
local layoutbox = require("lib/widgets/layoutbox")
local tasklist = require("lib/widgets/tasklist")
local clock = require("lib/widgets/clock")
local systray = require("lib/widgets/systray")

local sound_widget = sound()
local battery_widget = battery()
local systray_widget = systray()

return {
   mk_bar = function (screen, modkey)
      bar_wibox = awful.wibar({
	    position = "left",
	    screen = screen,
	    width = dpi(55),
      })

      promptbox = awful.widget.prompt()
      screen.promptbox = promptbox

      bar_wibox:setup {
	 layout = wibox.layout.align.vertical,
	 expand = "outside",
	 { 
            layout = wibox.layout.fixed.vertical,
            -- startmenu.launcher,
            promptbox,
	    tasklist(screen),
	 },
	 taglist(screen, modkey),
	 {
            layout = wibox.layout.align.vertical,
	    wibox.widget {},
	    wibox.widget {},
            { layout = wibox.layout.fixed.vertical,
	      sound_widget,
	      battery_widget,
	      systray_widget,
	      clock.clock,
	      layoutbox(screen),
	    },
	 },
      }
      return bar_wibox
   end,
}

