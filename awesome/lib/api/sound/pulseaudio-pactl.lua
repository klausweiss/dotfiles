local awful = require("awful")
local gears = require("gears")

local is_speakers_cmd = "pactl list sinks | grep -i active | grep speaker"
local is_output_muted = "pactl list sinks | grep -i 'mute: yes'"
local is_input_muted = "pactl list sources | grep -i 'mute: yes'"

local mute_input_cmd = "pactl set-source-mute @DEFAULT_SOURCE@ 1"
local unmute_input_cmd = "pactl set-source-mute @DEFAULT_SOURCE@ 0"
local toggle_mute_input_cmd = "pactl set-source-mute @DEFAULT_SOURCE@ toggle"

local mute_output_cmd = "pactl set-sink-mute @DEFAULT_SINK@ 1"
local unmute_output_cmd = "pactl set-sink-mute @DEFAULT_SINK@ 0"
local toggle_mute_output_cmd = "pactl set-sink-mute @DEFAULT_SINK@ toggle"

local get_sink_number_cmd =
"pactl list sinks | grep 'Sink #.' -A1 | grep RUNNING -B1 | head -n1 | sed 's/.*\\([0-9]\\+\\).*/\\1/g' | head -n1"
local get_all_sinks_numbers_cmd = "pactl list sinks | grep 'Sink #.' | sed 's/.*\\([0-9]\\+\\).*/\\1/g'"

local set_output_type_to_headphones_cmd = function(sink_number)
   return "pactl set-sink-port " ..
       sink_number .. " analog-output-headphones"
end
local set_output_type_to_speakers_cmd = function(sink_number)
   return "pactl set-sink-port " ..
       sink_number .. " analog-output-speaker"
end

local set_sink_number_cmd = function(sink_number) return "pactl set-default-sink " .. sink_number end

local get_output_volume_cmd = function(sink_number)
   return "pactl list sinks | sed -e '0,/Sink.*" ..
       sink_number .. "/d;/Sink/q' | grep -i 'volume.*front' | tail -n1"
end
local get_input_volume_cmd = "pactl list sources | grep -i 'volume.*front' | tail -n1"

local volume_up = function(x) return "pactl set-sink-volume @DEFAULT_SINK@ +" .. x .. "%" end
local volume_down = function(x) return "pactl set-sink-volume @DEFAULT_SINK@ -" .. x .. "%" end

-- split("a,b,c", ",") => {"a", "b", "c"}
local function split(s, sep)
   local fields = {}

   local sep = sep or " "
   local pattern = string.format("([^%s]+)", sep)
   string.gsub(s, pattern, function(c) fields[#fields + 1] = c end)

   return fields
end

function mk_sound_info()
   local sound_info = {
      output_volume = 50,
      output_muted = false,
      output_device_type = "speakers",
      sink_number = "0",
      input_volume = 50,
      input_muted = true,
   }

   function sound_info:emit_changed(what)
      awesome.emit_signal("sound_api:changed:" .. what)
   end

   function sound_info:refresh_output_muted(force)
      if not force and self._refreshing_output_muted then return end
      self._refreshing_output_muted = true

      awful.spawn.easy_async_with_shell
      (is_output_muted, function(o, e, r, code)
         self.output_muted = (code == 0)

         self._refreshing_output_muted = false
         self:emit_changed("output")
      end)
   end

   function sound_info:refresh_output_type(force)
      if not force and self._refreshing_output_type then return end
      self._refreshing_output_type = true

      awful.spawn.easy_async_with_shell
      (is_speakers_cmd, function(o, e, r, code)
         if code == 0 then
            self.output_device_type = "speakers"
         else
            self.output_device_type = "headphones"
         end
         self._refreshing_output_type = false
         self:emit_changed("output")
      end)
   end

   function sound_info:refresh_input_muted(force)
      if not force and self._refreshing_input_muted then return end
      self._refreshing_input_muted = true

      awful.spawn.easy_async_with_shell
      (is_input_muted, function(o, e, r, code)
         self.input_muted = (code == 0)

         self._refreshing_input_muted = false
         self:emit_changed("input")
      end)
   end

   local function get_volume_from_two_percents_string(str)
      local left, right = string.match(str, '.+ (%d+)%% .* (%d+)%%')
      left = tonumber(left)
      right = tonumber(right)
      if left == nil then
         local naughty = require("naughty")
         naughty.notify { text = str, timeout = 0 }
      end
      return math.floor((left + right) / 2)
   end

   function sound_info:refresh_output_volume(args)
      local args = args or {}
      if not args.force and self._refreshing_output_volume then return end
      self._refreshing_output_volume = true

      awful.spawn.easy_async_with_shell
      (get_output_volume_cmd(self.sink_number), function(out, e, r, c)
         self.output_volume = get_volume_from_two_percents_string("refresh output " .. out)

         self._refreshing_output_volume = false
         if not args.nonotify then
            self:emit_changed("output::volume")
         end
      end)
   end

   function sound_info:refresh_input_volume(args)
      local args = args or {}
      if not args.force and self._refreshing_input_volume then return end
      self._refreshing_input_volume = true

      awful.spawn.easy_async_with_shell
      (get_input_volume_cmd, function(out, e, r, c)
         self.input_volume = get_volume_from_two_percents_string("refresh input " .. out)

         self._refreshing_input_volume = false
         if not args.nonotify then
            self:emit_changed("input::volume")
         end
      end)
   end

   function sound_info:refresh_sink_number(force)
      if not force and self._refreshing_sink_number then return end
      self._refreshing_sink_number = true

      awful.spawn.easy_async_with_shell
      (get_sink_number_cmd, function(out, e, r, c)
         self.sink_number = out
         self._refreshing_sink_number = false
      end)
   end

   function sound_info:refresh_all_sinks_numbers(force)
      if not force and self._refreshing_all_sinks_numbers then return end
      self._refreshing_all_sinks_numbers = true

      awful.spawn.easy_async_with_shell
      (get_all_sinks_numbers_cmd, function(out, e, r, c)
         self.all_sinks_numbers = split(out, "\n")
         self._refreshing_all_sinks_numbers = false
      end)
   end

   function sound_info:refresh(args)
      self:refresh_input_muted(args.force)
      self:refresh_input_volume(args or {})
      self:refresh_output_muted(args.force)
      self:refresh_output_type(args.force)
      self:refresh_output_volume(args or {})
      self:refresh_sink_number(args.force)
      self:refresh_all_sinks_numbers(args.force)
   end

   function sound_info:get_input_device_volume() return self.input_volume end

   function sound_info:is_input_muted() return self.input_muted end

   function sound_info:get_output_device_volume() return self.output_volume end

   function sound_info:is_output_muted() return self.output_muted end

   function sound_info:get_output_device_type() return self.output_device_type end

   function sound_info:set_output_type_to_headphones() awful.spawn(set_output_type_to_headphones_cmd(self.sink_number)) end

   function sound_info:set_output_type_to_speakers() awful.spawn(set_output_type_to_speakers_cmd(self.sink_number)) end

   function sound_info:set_sink(sink_number)
      awful.spawn(set_sink_number_cmd(sink_number))
      self.sink_number = sink_number
   end

   function sound_info:find_next_sink()
      local prev_matched = false
      for k, v in pairs(self.all_sinks_numbers) do
         if prev_matched then
            return v
         end
         if v == self.sink_number then
            prev_matched = true
         end
      end
      return self.all_sinks_numbers[1]
   end

   return sound_info
end

function mk_api(args)
   local timeout = args.timeout
   local sound_info = mk_sound_info()
   local refresh_timer = gears.timer {
      timeout = timeout,
      callback = function()
         sound_info:refresh { nonotify = true }
      end,
      autostart = true,
   }

   return {
      get_input_device_volume = function() return sound_info:get_input_device_volume() end,
      is_input_muted = function() return sound_info:is_input_muted() end,
      mute_input = function()
         awful.spawn(mute_input_cmd)
         sound_info:refresh_input_muted()
      end,
      unmute_input = function()
         awful.spawn(unmute_input_cmd)
         sound_info:refresh_input_muted()
      end,
      toggle_mute_input = function()
         awful.spawn(toggle_mute_input_cmd)
         sound_info:refresh_input_muted()
      end,
      get_output_device_volume = function() return sound_info:get_output_device_volume() end,
      is_output_muted = function() return sound_info:is_output_muted() end,
      mute_output = function()
         awful.spawn(mute_output_cmd)
         sound_info:refresh_output_muted()
      end,
      unmute_output = function()
         awful.spawn(unmute_output_cmd)
         sound_info:refresh_output_muted()
      end,
      toggle_mute_output = function()
         awful.spawn(toggle_mute_output_cmd)
         sound_info:refresh_output_muted()
      end,
      get_output_device_type = function() return sound_info:get_output_device_type() end,
      set_output_type_to_speakers = function()
         sound_info:set_output_type_to_speakers()
         sound_info:refresh_output_type()
      end,
      set_output_type_to_headphones = function()
         sound_info:set_output_type_to_headphones()
         sound_info:refresh_output_type()
      end,
      toggle_output_type = function()
         if sound_info.output_device_type == "speakers" then
            sound_info:set_output_type_to_headphones()
         else
            sound_info:set_output_type_to_speakers()
         end
         sound_info:refresh_output_type()
      end,
      toggle_sink = function()
         sound_info:set_sink(sound_info:find_next_sink())
         sound_info:refresh_all_sinks_numbers()
      end,
      output_volume_up = function(x)
         awful.spawn(volume_up(x))
         sound_info:refresh_output_volume()
      end,
      output_volume_down = function(x)
         awful.spawn(volume_down(x))
         sound_info:refresh_output_volume()
      end,
      refresh = function(args)
         sound_info:refresh(args)
      end,
   }
end

return mk_api
