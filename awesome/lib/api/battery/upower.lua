local awful = require("awful")
local gears = require("gears")

local upower_battery_command = "sh -c \"upower -i /org/freedesktop/UPower/devices/DisplayDevice | grep -P '(state:|percentage:)'\""

function mk_battery_info()
   local battery_info = {
      percentage = 0,
      charging = false,

      _process_running = false,
   }
   function battery_info:refresh()
      if self._process_running then return end
      local callbacks = {
	 stdout = function(line)
	    local status = string.match(line, 'state:%s*(.+)')
	    if status ~= nil then 
	       self.charging = string.find(status, "discharging") == nil
	    else
	       local percentage_str = string.match(line, 'percentage:%s*(%d+)%%')
	       self.percentage = tonumber(percentage_str)
	    end

	    awesome.emit_signal "battery_api:refreshed"
	 end,
	 exit = function() self._process_running = false end,
      }
      self._refreshed = false
      self._process_running = true
      awful.spawn.with_line_callback(upower_battery_command, callbacks)
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
