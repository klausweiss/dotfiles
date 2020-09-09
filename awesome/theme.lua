local beautiful = require("beautiful")
local gears = require("gears")

local libtheme = require("lib/theme")
local libpath = require("lib/path")

local theme_path = libpath.expandhome("~/.config/awesome/themes/natty/theme.lua")
local wallpapaper_path = libpath.expandhome("~/.config/wallpaper")
beautiful.init(theme_path)
beautiful.wallpaper = wallpapaper_path

-- Re-set wallpaper when a screen's geometry changes (e.g. different resolution)
screen.connect_signal("property::geometry", libtheme.set_wallpaper)
