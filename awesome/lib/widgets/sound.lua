local awful = require("awful")
local beautiful = require("beautiful")
local gears = require("gears")
local wibox = require("wibox")
local dpi = require("beautiful.xresources").apply_dpi

local sound_api = require("lib/api/sound").instance

local icons = {
   headphones = "",
   speakers = "",
}

local font = "FontAwesome " .. dpi(20)

function get_sound_icon_color(muted)
   if muted then
      return beautiful.fg_ternary
   else
      return beautiful.fg_secondary
   end
end

function get_output_markup(args)
   local device_type = args.device_type
   local color = get_sound_icon_color(args.muted)
   local icon = icons[device_type]
   return "<span foreground='" .. color .. "'>" .. icon .. "</span>"
end

function get_input_markup(args)
   local color = get_sound_icon_color(args.muted)
   local icon = ""
   return "<span foreground='" .. color .. "'>" .. icon .. "</span>"
end

function mk_output_widget(api)
   local output = wibox.widget {
      markup  = nil,
      align   = "center",
      valign  = "center",
      font    = font,
      widget  = wibox.widget.textbox,
      buttons = gears.table.join(
         awful.button({}, 1, api.toggle_mute_output),
         awful.button({}, 2, api.toggle_output_type),
         awful.button({}, 3, api.toggle_sink),
         awful.button({}, 4, function() api.output_volume_up(5) end),
         awful.button({}, 5, function() api.output_volume_down(5) end)
      ),
   }
   function output:refresh()
      self.markup = get_output_markup {
         device_type = api.get_output_device_type(),
         muted = api.is_output_muted(),
      }
   end

   return output
end

function mk_input_widget(api)
   local input = wibox.widget {
      markup  = nil,
      align   = "center",
      valign  = "center",
      font    = font,
      widget  = wibox.widget.textbox,
      buttons = gears.table.join(
         awful.button({}, 1, api.toggle_mute_input)
      ),
   }
   function input:refresh()
      self.markup = get_input_markup {
         muted = api.is_input_muted(),
      }
   end

   return input
end

function add_sound_margins(w)
   return {
      w,
      bottom = dpi(5),
      top = dpi(5),
      widget = wibox.container.margin,
   }
end

local function mk_tooltip(widget, percentage_getter)
   local sound_tooltip = awful.tooltip {
      objects = { widget },
      timer_function = function()
         return percentage_getter() .. "%"
      end,
   }
   sound_tooltip.margins = dpi(10)
   return sound_tooltip
end


function mk_widget()
   local input_widget = mk_input_widget(sound_api)
   local output_widget = mk_output_widget(sound_api)
   awesome.connect_signal("sound_api:changed:output",
      function() output_widget:refresh() end)
   awesome.connect_signal("sound_api:changed:input",
      function() input_widget:refresh() end)
   sound_api.refresh { nonotify = true }
   local output_tooltip = mk_tooltip(output_widget, sound_api.get_output_device_volume)
   local input_tooltip = mk_tooltip(input_widget, sound_api.get_input_device_volume)
   widget = {
      add_sound_margins(input_widget),
      add_sound_margins(output_widget),
      layout = wibox.layout.flex.vertical,
   }
   return widget
end

return mk_widget
