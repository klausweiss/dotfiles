local awful = require("awful")

return {
   next_layout = function () awful.layout.inc( 1) end,
   prev_layout = function () awful.layout.inc(-1) end,
   select_main_client = function (client_) client_:swap(awful.client.getmaster()) end,
}
