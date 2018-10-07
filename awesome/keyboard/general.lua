local gears = require("gears")
local awful = require("awful")
local hotkeys_popup = require("awful.hotkeys_popup").widget

local menu = require("menu")

local general_keys = gears.table.join(
  awful.key({ modkey,       }, "s",    hotkeys_popup.show_help,
            {description="show help", group="awesome"}),
  awful.key({ modkey,       }, "Escape", awful.tag.history.restore,
            {description = "go back", group = "tag"}),
  awful.key({ modkey,       }, "w", function () menu.mainmenu:show() end,
            {description = "show main menu", group = "awesome"})
)

local special_keys = gears.table.join(
  awful.key({               }, "XF86AudioRaiseVolume",
    function ()
      awful.util.spawn("amixer sset Master 4%+")
    end,    {description = "raise volume", group = "hardware"}),

  awful.key({               }, "XF86AudioLowerVolume",
    function ()
      awful.util.spawn("amixer sset Master 4%-")
    end,    {description = "lower volume", group = "hardware"}),

  awful.key({                   }, "XF86MonBrightnessUp", 
    function () 
      awful.spawn("light -A 8") 
    end, {description = "monitor brightness up", group = "hardware"}),

  awful.key({                   }, "XF86MonBrightnessDown", 
    function () 
      awful.spawn("light -U 8") 
    end, {description = "monitor brightness down", group = "hardware"})
)

local globalkeys = gears.table.join(
  general_keys,
  special_keys
)

return {
  globalkeys = globalkeys
}
