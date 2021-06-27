local awful = require("awful")
local beautiful = require("beautiful")
local dpi = require("beautiful.xresources").apply_dpi
local hotkeys_popup = require("awful.hotkeys_popup")
local programs = require("programs")
local wibox = require("wibox")

local myawesomemenu = {
   { "hotkeys", function() hotkeys_popup.show_help(nil, awful.screen.focused()) end },
   { "manual", programs.terminal .. " -e man awesome" },
   { "edit config", programs.editor .. " " .. awesome.conffile },
   { "restart", awesome.restart },
   { "quit", function() awesome.quit() end },
}


local menu_awesome = { "awesome", myawesomemenu, beautiful.awesome_icon }
local menu_terminal = { "open terminal", terminal }


local has_fdo, freedesktop = pcall(require, "freedesktop")
local has_debian, debian = pcall(require, "debian.menu")

if has_fdo then
   mymainmenu = freedesktop.menu.build({
	 before = { menu_awesome },
	 after =  { menu_terminal }
   })
elseif has_debian then
   mymainmenu = awful.menu({
	 items = {
	    menu_awesome,
	    { "Debian", debian.menu.Debian_menu.Debian },
	    menu_terminal,
	 }
   })
else
   mymainmenu = awful.menu({
	 items = {
	    menu_awesome,
	    menu_terminal,
	 }
   })
end

icon_launcher = awful.widget.launcher({ image = beautiful.awesome_icon,
					menu = mymainmenu })
local mylauncher = wibox.widget {
   {
      icon_launcher,
      margins = dpi(10),
      widget = wibox.container.margin,
   },
   layout = wibox.layout.stack
}

return {
   mainmenu = mymainmenu,
   launcher = mylauncher,
}
