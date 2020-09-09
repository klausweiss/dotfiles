local awful = require("awful")

return {
   focus_next  = function () awful.client.focus.byidx( 1) end,
   focus_prev  = function () awful.client.focus.byidx(-1) end,
   focus_up    = function () awful.client.focus.bydirection("up") end,
   focus_right = function () awful.client.focus.bydirection("right") end,
   focus_down  = function () awful.client.focus.bydirection("down") end,
   focus_left  = function () awful.client.focus.bydirection("left") end,

   toggle_focus_minimize_client = function (client_)
      if client_ == client.focus then
	 client_.minimized = true
      else
	 client_.minimized = false
	 if not client_:isvisible() and client_.first_tag then
	    client_.first_tag:view_only()
	 end
	 client.focus = client_
	 client_:raise()
      end
   end,

   toggle_fullscreen_client = function (client_)
      client_.fullscreen = not client_.fullscreen
      client_:raise()
   end,

   toggle_maximize_client = function (client_)
      client_.maximized = not client_.maximized
      client_:raise()
   end,

   close_client = function (client_)
      client_:kill()
   end,
}

