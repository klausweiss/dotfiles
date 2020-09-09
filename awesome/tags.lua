local awful = require("awful")

local focused_screen = function () return awful.screen.focused() end

local with_current_tag = function (f)
   return function (tag)
      if not tag then tag = focused_screen().selected_tag end
      return f(tag)
   end
end

local by_index = function (f)
   return function (index)
      return function ()
	 local tag = focused_screen().tags[index]
	 return f(tag)
      end
   end
end

return {
   tags = {"1", "2", "3", "4", "5", "6"},
   
   show_only_tag       = function (tag) tag:view_only() end,
   show_tag_by_index   = by_index(function (tag) tag:view_only() end),
   show_next_tag       = with_current_tag(function (tag) awful.tag.viewnext(tag.screen) end),
   show_prev_tag       = with_current_tag(function (tag) awful.tag.viewprev(tag.screen) end),
   toggle_tag          = awful.tag.viewtoggle,
   toggle_tag_by_index = by_index(awful.tag.viewtoggle),

   move_focused_client_to_tag   = by_index(function (tag)
	 if client.focus then client.focus:move_to_tag(tag) end
   end),
   toggle_tag_on_focused_client = by_index(function (tag)
	 if client.focus then client.focus:toggle_tag(tag) end
   end),
}
