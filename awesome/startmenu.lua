local awful = require("awful")
local beautiful = require("beautiful")
local hotkeys_popup = require("awful.hotkeys_popup")
local programs = require("programs")

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

if has_fdo then
   mymainmenu = freedesktop.menu.build({
	 before = { menu_awesome },
	 after =  { menu_terminal }
   })
else
   mymainmenu = awful.menu({
	 items = {
	    menu_awesome,
	    { "Debian", debian.menu.Debian_menu.Debian },
	    menu_terminal,
	 }
   })
end

mylauncher = awful.widget.launcher({ image = beautiful.awesome_icon,
                                     menu = mymainmenu })

return {
   mainmenu = mymainmenu,
   launcher = mylauncher,
}