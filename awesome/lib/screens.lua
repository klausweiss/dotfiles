local awful = require("awful")

local function focused_screen()
   return awful.screen.focused()
end

local function get_screen_by_name(name)
   return function()
      local any_screen = nil -- in case screen with given name couldn't be found
      for s in screen do
         if s.outputs[name] ~= nil then
            return s
         end
         any_screen = s
      end
      return any_screen
   end
end

return {
   focus_left_screen           = function() awful.screen.focus_bydirection("left", focused_screen()) end,
   focus_right_screen          = function() awful.screen.focus_bydirection("right", focused_screen()) end,
   focus_down_screen           = function() awful.screen.focus_bydirection("down", focused_screen()) end,
   focus_up_screen             = function() awful.screen.focus_bydirection("up", focused_screen()) end,
   move_client_to_left_screen  = function()
      if client.focus then
         client.focus:move_to_screen(focused_screen()
         :get_next_in_direction("left"))
      end
   end,
   move_client_to_right_screen = function()
      if client.focus then
         client.focus:move_to_screen(focused_screen()
         :get_next_in_direction("right"))
      end
   end,
   move_client_to_down_screen  = function()
      if client.focus then
         client.focus:move_to_screen(focused_screen()
         :get_next_in_direction("down"))
      end
   end,
   move_client_to_up_screen    = function()
      if client.focus then
         client.focus:move_to_screen(focused_screen()
         :get_next_in_direction("up"))
      end
   end,
   get_screen_by_name          = get_screen_by_name,
}
