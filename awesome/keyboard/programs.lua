local gears = require("gears")
local awful = require("awful")

-- Standard program
local globalkeys = gears.table.join(
  awful.key({ modkey, "Shift"   }, "Return", function () awful.spawn(terminal) end,
    {description = "open a terminal", group = "launcher"}),
  awful.key({ modkey, "Shift"   }, "e", function () awful.spawn(file_manager) end,
    {description = "open a file manager", group = "launcher"}),
  awful.key({ modkey, "Shift"   }, "d", function () awful.spawn(mail_client) end,
    {description = "open a mail client", group = "launcher"}),
  awful.key({ modkey, "Shift"   }, "b", function () awful.spawn(internet_browser) end,
    {description = "open an internet browser", group = "launcher"}),
-- Hardware
  awful.key({                   }, "XF86MonBrightnessUp", function () awful.spawn("light -A 8") end,
    {description = "monitor brightness up", group = "hardware"}),
  awful.key({                   }, "XF86MonBrightnessDown", function () awful.spawn("light -U 8") end,
    {description = "monitor brightness down", group = "hardware"}),
-- Awesome
  awful.key({ modkey, "Shift"   }, "r", awesome.restart,
    {description = "reload awesome", group = "awesome"}),
  awful.key({ modkey, "Shift"   }, "q", awesome.quit,
    {description = "quit awesome", group = "awesome"})
)

return {
  globalkeys = globalkeys
}
