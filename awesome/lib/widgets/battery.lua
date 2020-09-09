local beautiful = require("beautiful")
local gears = require("gears")
local wibox = require("wibox")
local dpi = require("beautiful.xresources").apply_dpi

local battery_api = require("lib/api/battery").instance

local icons = {
   "",
   "",
   "",
   "",
   "",
   "",
}

local font = "FontAwesome " .. dpi(20)

function get_battery_icon_color(percentage, is_charging)
   if is_charging then
      return beautiful.fg_ternary
   else
      if percentage > 5 then
	 return beautiful.fg_primary
      else
	 return beautiful.fg_alert
      end
   end
end

function get_percentage_markup(args)
   local percentage = args.percentage
   local is_charging = args.is_charging
   local color = get_battery_icon_color(percentage, is_charging)
   local icon_index = math.max(0, math.floor((percentage - 1 + 7) / (107 / #icons))) + 1
   local icon = icons[icon_index]
   return "<span foreground='" .. color .. "'>" .. icon .."</span>"
end

local battery = wibox.widget {
   markup = nil,
   align  = "center",
   valign = "center",
   widget = wibox.widget.textbox,
}
function battery:refresh(api)
   self.markup = get_percentage_markup {
      percentage = api.get_percentage(),
      is_charging = api.is_charging(),
   }
end

function add_battery_margins(w)
   return {
      w,
      bottom = dpi(5),
      top = dpi(5),
      widget = wibox.container.margin,
   }
end


function mk_widget()
   awesome.connect_signal("battery_api:refreshed",
			  function ()
			     battery:refresh(battery_api)
   end)
   battery_api:refresh()
   return {
      add_battery_margins(battery),
      layout = wibox.layout.flex.vertical,
   }
end

return mk_widget
