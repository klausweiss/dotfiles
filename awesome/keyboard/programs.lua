local gears = require("gears")
local awful = require("awful")

-- Standard program
local globalkeys = gears.table.join(
  awful.key({ modkey,           }, "Return", function () awful.spawn(terminal) end,
    {description = "open a terminal", group = "launcher"}),
  awful.key({ modkey, "Shift"   }, "e", function () awful.spawn(file_manager) end,
    {description = "open a file manager", group = "launcher"}),
  awful.key({ modkey, "Shift"   }, "d", function () awful.spawn(mail_client) end,
    {description = "open a mail client", group = "launcher"}),
  awful.key({ modkey, "Shift"   }, "b", function () awful.spawn(internet_browser) end,
    {description = "open an internet browser", group = "launcher"}),
-- Awesome
  awful.key({ modkey, "Shift"   }, "r", awesome.restart,
    {description = "reload awesome", group = "awesome"}),
  awful.key({ modkey, "Shift"   }, "q", awesome.quit,
    {description = "quit awesome", group = "awesome"})
)

return {
  globalkeys = globalkeys
}
