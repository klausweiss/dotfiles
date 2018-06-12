local gears = require("gears")
local awful = require("awful")

local globalkeys = gears.table.join(
  -- Layout manipulation
  awful.key({ modkey, "Shift"   }, "j", function () awful.client.swap.byidx(  1) end,
        {description = "swap with next client by index", group = "client"}),
  awful.key({ modkey, "Shift"   }, "k", function () awful.client.swap.byidx( -1) end,
        {description = "swap with previous client by index", group = "client"}),
  awful.key({ modkey, "Control" }, "j", function () awful.screen.focus_relative( 1) end,
        {description = "focus the next screen", group = "screen"}),
  awful.key({ modkey, "Control" }, "k", function () awful.screen.focus_relative(-1) end,
        {description = "focus the previous screen", group = "screen"}),
  awful.key({ modkey,           }, "u", awful.client.urgent.jumpto,
        {description = "jump to urgent client", group = "client"}),
  awful.key({ modkey,           }, "Tab",
    function ()
      awful.client.focus.history.previous()
      if client.focus then
        client.focus:raise()
      end
    end,
        {description = "go back", group = "client"}),

  awful.key({ modkey,           }, "l",   function () awful.tag.incmwfact( 0.05) end,
        {description = "increase master width factor", group = "layout"}),
  awful.key({ modkey,           }, "h",   function () awful.tag.incmwfact(-0.05) end,
        {description = "decrease master width factor", group = "layout"}),
  awful.key({ modkey, "Shift"   }, "h",   function () awful.tag.incnmaster( 1, nil, true) end,
        {description = "increase the number of master clients", group = "layout"}),
  awful.key({ modkey, "Shift"   }, "l",   function () awful.tag.incnmaster(-1, nil, true) end,
        {description = "decrease the number of master clients", group = "layout"}),
  awful.key({ modkey, "Control" }, "h",   function () awful.tag.incncol( 1, nil, true) end,
        {description = "increase the number of columns", group = "layout"}),
  awful.key({ modkey, "Control" }, "l",   function () awful.tag.incncol(-1, nil, true) end,
        {description = "decrease the number of columns", group = "layout"}),
  awful.key({ modkey,           }, "space", function () awful.layout.inc( 1) end,
        {description = "select next", group = "layout"}),
  awful.key({ modkey, "Shift"   }, "space", function () awful.layout.inc(-1) end,
        {description = "select previous", group = "layout"}),

  awful.key({ modkey,           }, "j",   function () awful.client.focus.byidx( 1) end,
        {description = "focus next by index", group = "client"}),
  awful.key({ modkey,           }, "k",   function () awful.client.focus.byidx(-1) end,
        {description = "focus previous by index", group = "client"}),

  awful.key({ modkey, "Control" }, "n",
    function ()
      local c = awful.client.restore()
      -- Focus restored client
      if c then
        client.focus = c
        c:raise()
      end
    end,
        {description = "restore minimized", group = "client"})
)

return {
  globalkeys = globalkeys
}
