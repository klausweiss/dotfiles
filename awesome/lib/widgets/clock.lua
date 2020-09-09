local wibox = require("wibox")
local dpi = require("beautiful.xresources").apply_dpi

local mytextclock = wibox.widget.textclock()
mytextclock.format = [[
<b>%H
%M</b>]]
mytextclock.align = 'center'
mytextclock.font = 'Open Sans ' .. dpi(20)

local widget = {
   mytextclock,
   top = dpi(5),
   left = dpi(6),
   right = dpi(6),
   bottom = dpi(5),
   widget = wibox.container.margin,
}

return {
   clock = widget,
}
