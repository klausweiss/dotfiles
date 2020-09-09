local awful = require("awful")
local beautiful = require("beautiful")
local gears = require("gears")
local wibox = require("wibox")
local dpi = require("beautiful.xresources").apply_dpi


local tasklist_buttons = gears.table.join(
   awful.button({ }, 1, function (c)
	 if c == client.focus then
	    c.minimized = true
	 else
	    c:emit_signal(
	       "request::activate",
	       "tasklist",
	       {raise = true}
	    )
	 end
   end),
   awful.button({ }, 3, function()
	 awful.menu.client_list({ theme = { width = 250 } })
   end),
   awful.button({ }, 4, function ()
	 awful.client.focus.byidx(1)
   end),
   awful.button({ }, 5, function ()
	 awful.client.focus.byidx(-1)
end))


function tasklist_selected_bg()
   return beautiful.tasklist_bg_focus or beautiful.bg_focus
end

function tasklist_normal_bg()
   return beautiful.tasklist_bg_normal or beautiful.bg_normal
end

function tasklist_minimized_bg()
   return beautiful.tasklist_bg_minimize or beautiful.bg_minimize
end

function task_element_callback (self, c, index, clients)
   local square_bg = self:get_children_by_id('bg')[1]
   if c == client.focus then
      square_bg.bg = tasklist_selected_bg()
   elseif c.minimized then
      square_bg.bg = tasklist_minimized_bg()
   else
      square_bg.bg = tasklist_normal_bg()
   end
end

function mk_tasklist (screen)
   return awful.widget.tasklist {
      screen  = screen,
      filter  = awful.widget.tasklist.filter.currenttags,
      buttons = tasklist_buttons,
      layout = {
	 layout = wibox.layout.fixed.vertical,
      },
      widget_template = {
	 {
	    {
	       {
		  id     = 'icon_role',
		  widget = wibox.widget.imagebox,
	       },
	       margins = dpi(10),
	       widget  = wibox.container.margin,
	    },
	    id     = 'bg',
	    widget = wibox.container.background,
	 },

	 widget  = wibox.container.margin,

	 create_callback = task_element_callback,
	 update_callback = task_element_callback,
      },
   }
end

return mk_tasklist
