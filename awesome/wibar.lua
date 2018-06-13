local gears = require("gears")
local awful = require("awful")
local wibox = require("wibox")

local menu = require("menu")

local volumebar = require("widgets.volumebar")
-- Keyboard map indicator and switcher
local keyboardlayout = awful.widget.keyboardlayout()
-- Create a textclock widget
local textclock = wibox.widget.textclock()


-- Create a wibox for each screen and add it
local taglist_buttons = gears.table.join(
          awful.button({ }, 1, function(t) t:view_only() end),
          awful.button({ modkey }, 1, function(t)
                        if client.focus then
                          client.focus:move_to_tag(t)
                        end
                      end),
          awful.button({ }, 3, awful.tag.viewtoggle),
          awful.button({ modkey }, 3, function(t)
                        if client.focus then
                          client.focus:toggle_tag(t)
                        end
                      end),
          awful.button({ }, 4, function(t) awful.tag.viewnext(t.screen) end),
          awful.button({ }, 5, function(t) awful.tag.viewprev(t.screen) end)
        )

local tasklist_buttons = gears.table.join(
           awful.button({ }, 1, function (c)
                        if c == client.focus then
                          c.minimized = true
                        else
                          -- Without this, the following
                          -- :isvisible() makes no sense
                          c.minimized = false
                          if not c:isvisible() and c.first_tag then
                            c.first_tag:view_only()
                          end
                          -- This will also un-minimize
                          -- the client, if needed
                          client.focus = c
                          c:raise()
                        end
                      end),
           awful.button({ }, 3, menu.client_menu_toggle_fn()),
           awful.button({ }, 4, function ()
                        awful.client.focus.byidx(1)
                      end),
           awful.button({ }, 5, function ()
                        awful.client.focus.byidx(-1)
                      end))


return {
  keyboardlayout = keyboardlayout,
  textclock = textclock,
  taglist_buttons = taglist_buttons,
  tasklist_buttons = tasklist_buttons,
  volumebar = volumebar
}
