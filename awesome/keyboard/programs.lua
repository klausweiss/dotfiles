local gears = require("gears")
local awful = require("awful")

-- Standard program
local globalkeys = gears.table.join(
  awful.key({ modkey, "Shift"   }, "Return", function () awful.spawn(terminal) end,
    {description = "open a terminal", group = "launcher"}),
  awful.key({ modkey, "Shift"   }, "r", awesome.restart,
    {description = "reload awesome", group = "awesome"}),
  awful.key({ modkey, "Shift"   }, "q", awesome.quit,
    {description = "quit awesome", group = "awesome"})
)

return {
  globalkeys = globalkeys
}
