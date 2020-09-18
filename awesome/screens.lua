local awful = require("awful")

local libtheme = require("lib/theme")
-- the layouts import is required for each screen to have correct layout
local layouts = require("layouts")
local bars = require("lib/bars")
local keyboard = require("keyboard")
local tags = require("tags")

awful.screen.connect_for_each_screen(function(screen)
    libtheme.set_wallpaper(screen)
    awful.tag(tags.tags, screen, awful.layout.layouts[1])
    bars.mk_bar(screen, keyboard.CTRL)
end)
