local awful = require("awful")
local beautiful = require("beautiful")
local gears = require("gears")
local naughty = require("naughty")
local dpi = require("beautiful.xresources").apply_dpi

local sound_api = require("lib/api/sound").instance

naughty.config["spacing"] = dpi(10)
naughty.config.defaults["margin"] = dpi(16)
naughty.config.defaults["width"] = dpi(400)
naughty.config.defaults["icon_size"] = dpi(100)
naughty.config.defaults["shape"] = function (...)
   local cr, w, h = ...
   return gears.shape.partially_rounded_rect(cr, w, h, true, true, true, true, dpi(10))
end
naughty.config.defaults["font"] = "JetBrains Mono " .. dpi(13)

naughty.config.presets.critical["bg"] = beautiful.bg_alert

local VOLUME_NOTIFICATION_ID = 123

local volume_notification = nil

local function on_volume_change()
   title = "Volume changed"
   text = sound_api.get_output_device_volume() .. "%"
   if not volume_notification then
      volume_notification = naughty.notify {
	 title = title,
	 text = text,
	 replaces_id = VOLUME_NOTIFICATION_ID,
	 destroy = function()
	    volume_notification = nil
	 end,
      }
   else
      naughty.replace_text(volume_notification, title, text)
      -- naughty.reset_timeout(volume_notification)
   end
end
awesome.connect_signal("sound_api:changed:output::volume", on_volume_change)
