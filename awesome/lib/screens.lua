local awful = require("awful")

return {
   focus_left_screen  = function () awful.screen.focus_bydirection("left",  awful.screen.focused()) end,
   focus_right_screen = function () awful.screen.focus_bydirection("right", awful.screen.focused()) end,
   move_client_to_next_screen = function () if client.focus then client.focus:move_to_screen() end end,
}
