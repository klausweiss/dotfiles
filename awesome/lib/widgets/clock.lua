local awful = require("awful")
local wibox = require("wibox")
local dpi = require("beautiful.xresources").apply_dpi

local function mk_widget()
   local clock_widget = wibox.widget.textclock()
   clock_widget.format = [[
<b>%H
%M</b>]]
   clock_widget.align = 'center'
   clock_widget.font = 'Open Sans ' .. dpi(20)

   local widget = {
      clock_widget,
      top = dpi(5),
      left = dpi(6),
      right = dpi(6),
      bottom = dpi(5),
      widget = wibox.container.margin,
   }

   local date_widget = wibox.widget.textclock()
   date_widget.format = "%a, %d.%m.%Y"

   local tooltip = awful.tooltip {
      objects = { clock_widget },
      timer_function = function ()
	 return date_widget.text
      end,
   }
   tooltip.margins = dpi(10)

   return widget
end


return mk_widget
