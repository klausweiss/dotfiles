local awful = require("awful")
local gears = require("gears")
local wibox = require("wibox")
local dpi = require("beautiful.xresources").apply_dpi


function mk_layoutbox (screen)
   box = awful.widget.layoutbox(screen)
   box:buttons(gears.table.join(
		  awful.button({ }, 1, function () awful.layout.inc( 1) end),
		  awful.button({ }, 3, function () awful.layout.inc(-1) end),
		  awful.button({ }, 4, function () awful.layout.inc( 1) end),
		  awful.button({ }, 5, function () awful.layout.inc(-1) end)))
   widget = {
      box,
      widget = wibox.container.margin,
      bottom = dpi(15),
      top = dpi(5),
      left = dpi(15),
      right = dpi(15),
   }
   return widget
end


return mk_layoutbox
