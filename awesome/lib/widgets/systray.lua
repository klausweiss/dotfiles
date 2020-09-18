local awful = require("awful")
local beautiful = require("beautiful")
local gears = require("gears")
local wibox = require("wibox")
local dpi = require("beautiful.xresources").apply_dpi

local icons = {
   expand = "",
   hide = "",
}

local font = "FontAwesome " .. dpi(24)


local function add_systray_margins(w)
   return {
      w,
      bottom = dpi(3),
      top = dpi(3),
      widget = wibox.container.margin,
   }
end

local function get_toggle_markup(shown)
   local color
   local icon
   if not shown then
      icon = icons.expand
      color = beautiful.fg_secondary
   else
      icon = icons.hide
      color = beautiful.fg_ternary
   end
   return "<span foreground='" .. color .. "'>" .. icon .."</span>"
end

local function mk_systray()
   local systray = wibox.widget.systray()
   systray.set_base_size(dpi(22))
   systray.visible = false

   systray.popup = awful.popup {
      widget = {
	 systray,
	 widget = wibox.container.margin,
	 top = dpi(16),
	 left = dpi(16),
	 right = dpi(32),
	 bottom = dpi(16),
      },
      ontop = true,
      visible = false,
   }
   return systray
end

local function mk_toggle_widget(args)
   args = args or {}
   local systray = args.systray
   local timeout = args.timeout or 1

   local toggle_widget = wibox.widget {
      align  = "center",
      valign = "center",
      widget = wibox.widget.textbox,
      font = font,
      expanded = false,
   }

   function toggle_widget:reset_leave_timer()
      if toggle_widget.expanded then
	 toggle_widget.hide_timer:again()
      end
   end

   function toggle_widget:step_leave_timer()
      if toggle_widget.hide_timer.started then
	 toggle_widget.hide_timer:stop()
      end
   end

   function toggle_widget:set_expanded(shown)
      self.expanded = shown
   end

   function toggle_widget:refresh()
      self.markup = get_toggle_markup(self.expanded)
   end

   local function on_expand()
      toggle_widget:set_expanded(true)
      toggle_widget:refresh()
      systray.popup:move_next_to(mouse.current_widget_geometry)
      systray:set_screen(awful.screen.focused())
      systray.popup.visible = true
      systray.visible = true
   end

   local function on_collapse()
      toggle_widget:set_expanded(false)
      toggle_widget:refresh()
      systray.popup.visible = false
      systray.visible = false
   end

   toggle_widget.hide_timer = gears.timer {
      timeout = timeout,
      single_shot = true,
      callback = on_collapse,
   }

   toggle_widget:buttons(
      gears.table.join(
	 awful.button({ }, 1, function()
	       if toggle_widget.expanded then
		  on_collapse()
	       else
		  on_expand()
	       end
	 end)
   ))

   toggle_widget:connect_signal('mouse::leave', toggle_widget.reset_leave_timer)
   toggle_widget:connect_signal('mouse::enter', toggle_widget.step_leave_timer)
   systray.popup:connect_signal('mouse::leave', toggle_widget.reset_leave_timer)
   systray.popup:connect_signal('mouse::enter', toggle_widget.step_leave_timer)

   toggle_widget:refresh()
   return toggle_widget
end


local function mk_widget ()
   local toggle_widget = mk_toggle_widget {
      systray = mk_systray(),
   }
   return add_systray_margins(toggle_widget)
end


return mk_widget
