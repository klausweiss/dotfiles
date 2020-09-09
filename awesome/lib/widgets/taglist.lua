local awful = require("awful")
local beautiful = require("beautiful")
local gears = require("gears")
local wibox = require("wibox")
local dpi = require("beautiful.xresources").apply_dpi

local function mk_taglist_buttons(modkey)
   return gears.table.join(
      awful.button({ }, 1, function(t) t:view_only() end),
      awful.button({ modkey }, 1, function(t)
	    if client.focus then
	       client.focus:move_to_tag(t)
	    end
      end),
      awful.button({ }, 3, awful.tag.viewtoggle),
      awful.button({ modkey }, 3, function(t)
	    if client.focus then
	       client.focus:toggle_tag(t)
	    end
      end),
      awful.button({ }, 4, function(t) awful.tag.viewprev(t.screen) end),
      awful.button({ }, 5, function(t) awful.tag.viewnext(t.screen) end)
   )
end

local function taglist_fg()
   return beautiful.taglist_fg_normal or beautiful.fg_normal
end

local function taglist_bg()
   return beautiful.taglist_bg_normal or beautiful.bg_normal
end

local function dot_colors(args)
   fg = taglist_fg()
   bg = taglist_bg()

   if args.is_selected then
      return fg, fg
   elseif args.has_clients then
      return bg, fg
   else
      return bg, bg
   end
end

local function tag_element_callback (self, tag, index, tags)
   local is_selected_dot = self:get_children_by_id('is_selected_dot')[1]
   local has_clients_dot = self:get_children_by_id('has_clients_dot')[1]
   is_selected_dot.bg, has_clients_dot.bg = dot_colors {
      is_selected = tag.selected,
      has_clients = #tag:clients() > 0,
   }
end


local mk_taglist = function (screen, modkey)
   local taglist = awful.widget.taglist {
      screen = screen,
      filter  = awful.widget.taglist.filter.all,
      buttons = mk_taglist_buttons(modkey),
      layout = {
	 layout = wibox.layout.fixed.vertical,
      },
      widget_template = {
	 {
	    {
	       {
		  {
		     {
			{
			   margins = dpi(2),
			   widget  = wibox.container.margin,
			},
			id = 'has_clients_dot',
			shape  = gears.shape.circle,
			widget = wibox.container.background,
		     },
		     margins = dpi(4),
		     widget  = wibox.container.margin,
		  },
		  id     = 'is_selected_dot',
		  shape  = gears.shape.circle,
		  widget = wibox.container.background,
	       },
	       margins = dpi(3),
	       widget  = wibox.container.margin,
	    },
	    bg     = taglist_fg(),
	    shape  = gears.shape.circle,
	    widget = wibox.container.background,
	 },
	 margins = dpi(5),
	 widget = wibox.container.margin,
	 
	 create_callback = tag_element_callback,
	 update_callback = tag_element_callback,
      },
   }

   return {
      taglist,
      bottom = dpi(5),
      top = dpi(5),
      widget = wibox.container.margin,
   }
end

return mk_taglist
