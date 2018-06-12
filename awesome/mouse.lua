local gears = require("gears")
local awful = require("awful")

local menu = require("menu")


rootbuttons = gears.table.join(
  awful.button({ }, 3, function () menu.mainmenu:toggle() end),
  awful.button({ }, 4, awful.tag.viewnext),
  awful.button({ }, 5, awful.tag.viewprev)
)

clientbuttons = gears.table.join(
  awful.button({        }, 1, function (c) client.focus = c; c:raise() end),
  awful.button({ modkey }, 1, awful.mouse.client.move),
  awful.button({ modkey }, 3, awful.mouse.client.resize)
)

return {
  rootbuttons = rootbuttons,
  clientbuttons = clientbuttons
}
