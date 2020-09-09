local gears = require("gears")

function mk_sound_info()
   local sound_info = {
      output_volume = 10,
      output_muted = false,
      output_device_type = "speakers",
      input_volume = 50,
      input_muted = true,
   }
   function sound_info:refresh()
      self.output_muted = not self.output_muted
      self.input_muted = not self.input_muted
      if self.output_device_type == "speakers" then
	 self.output_device_type = "headphones"
      else
	 self.output_device_type = "speakers"
      end
      awesome.emit_signal "sound_api:refreshed"
   end
   function sound_info:get_input_device_volume() return self.input_volume end
   function sound_info:set_input_device_volume(x) self.input_volume = x end
   function sound_info:is_input_muted() return self.input_muted end
   function sound_info:get_output_device_type() return self.output_device_type end
   function sound_info:get_output_device_volume() return self.output_volume end
   function sound_info:set_output_device_volume(x) self.output_volume = x end
   function sound_info:is_output_muted() return self.output_muted end

   return sound_info
end

function mk_api(args)
   local timeout = args.timeout
   local sound_info = mk_sound_info()
   local refresh_timer = gears.timer {
      timeout = timeout,
      callback = function () sound_info:refresh() end,
      autostart = true,
   }
   return {
      get_input_device_volume = function() return sound_info:get_input_device_volume() end,
      set_input_device_volume = function(x) sound_info:set_input_device_volume(x) end,
      is_input_muted = function() return sound_info:is_input_muted() end,
      get_output_device_type = function() return sound_info:get_output_device_type() end,
      get_output_device_volume = function() return sound_info:get_output_device_volume() end,
      set_output_device_volume = function(x) sound_info:set_output_device_volume(x) end,
      is_output_muted = function() return sound_info:is_output_muted() end,
      refresh = function() sound_info:refresh() end,
   }
end

return mk_api
