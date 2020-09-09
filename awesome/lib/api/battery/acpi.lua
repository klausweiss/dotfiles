local awful = require("awful")
local gears = require("gears")

local battery_command = "acpi -i"


function mk_battery_info()
   local battery_info = {
      percentage = 0,
      charging = false,

      _process_running = false,
      _refreshed = false,
   }
   function battery_info:refresh()
      if self._process_running then return end
      local callbacks = {
	 stdout = function(line)
	    if self._refreshed then return end
	    local status, charge_str, time = string.match(line, '.+: (%a+), (%d?%d?%d)%%,?(.*)')
	    self.charging = string.find("Charging Full", status) ~= nil
	    self.percentage = tonumber(charge_str)
	    self._refreshed = true

	    awesome.emit_signal "battery_api:refreshed"
	 end,
	 exit = function() self._process_running = true end,
      }
      self._refreshed = false
      self._process_running = true
      awful.spawn.with_line_callback(battery_command, callbacks)
   end
   return battery_info
end

function mk_api(args)
   local timeout = args.timeout
   local battery_info = mk_battery_info()
   local refresh_timer = gears.timer {
      timeout = timeout,
      callback = function () battery_info:refresh() end,
      autostart = true,
   }
   return {
      get_percentage = function () return battery_info.percentage end,
      is_charging = function () return battery_info.charging end,
      refresh = function () battery_info:refresh() end,
   }
end

return mk_api
