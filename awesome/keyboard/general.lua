local gears = require("gears")
local awful = require("awful")
local hotkeys_popup = require("awful.hotkeys_popup").widget

local menu = require("menu")

local globalkeys = gears.table.join(
  awful.key({ modkey,       }, "s",    hotkeys_popup.show_help,
            {description="show help", group="awesome"}),
  awful.key({ modkey,       }, "Escape", awful.tag.history.restore,
            {description = "go back", group = "tag"}),
  awful.key({ modkey,       }, "w", function () menu.mainmenu:show() end,
            {description = "show main menu", group = "awesome"})
)

return {
  globalkeys = globalkeys
}
