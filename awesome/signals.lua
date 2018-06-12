local gears = require("gears")
local awful = require("awful")
local wibox = require("wibox")
local beautiful = require("beautiful")

local screens = require("screens")


-- Signals
-- Signal function to execute when a new client appears.
local function manage_handler(c)
  -- Set the windows at the slave,
  -- i.e. put it at the end of others instead of setting it master.
  -- if not awesome.startup then awful.client.setslave(c) end

  if awesome.startup and
  not c.size_hints.user_position
  and not c.size_hints.program_position then
  -- Prevent clients from being unreachable after screen count changes.
  awful.placement.no_offscreen(c)
  end
end

-- Add a titlebar if titlebars_enabled is set to true in the rules.
local function request_titlebars_handler(c)
  -- buttons for the titlebar
  local buttons = gears.table.join(
    awful.button({ }, 1, function()
      client.focus = c
      c:raise()
      awful.mouse.client.move(c)
    end),
    awful.button({ }, 3, function()
      client.focus = c
      c:raise()
      awful.mouse.client.resize(c)
    end)
  )

  awful.titlebar(c) : setup {
    { -- Left
      awful.titlebar.widget.iconwidget(c),
      buttons = buttons,
      layout  = wibox.layout.fixed.horizontal
    },
    { -- Middle
      { -- Title
        align  = "center",
        widget = awful.titlebar.widget.titlewidget(c)
      },
      buttons = buttons,
      layout  = wibox.layout.flex.horizontal
    },
    { -- Right
      awful.titlebar.widget.floatingbutton (c),
      awful.titlebar.widget.maximizedbutton(c),
      awful.titlebar.widget.stickybutton   (c),
      awful.titlebar.widget.ontopbutton  (c),
      awful.titlebar.widget.closebutton  (c),
      layout = wibox.layout.fixed.horizontal()
    },
    layout = wibox.layout.align.horizontal
  }
end

-- Enable sloppy focus, so that focus follows mouse.
local function mouse_enter_handler(c)
  if awful.layout.get(c.screen) ~= awful.layout.suit.magnifier
    and awful.client.focus.filter(c) then
    client.focus = c
  end
end

local function focus_handler(c)
  c.border_color = beautiful.border_focus
end

local function unfocus_handler(c)
  c.border_color = beautiful.border_normal
end


local client_signals = {
  {"manage", manage_handler},
  {"focus", focus_handler},
  {"unfocus", unfocus_handler},
  {"mouse::enter", mouse_enter_handler},
  {"request::titlebars", request_titlebars_handler},
}

local screen_signals = {
  {"property::geometry", screens.set_wallpaper},
}

local function _connect_signals(signals, target)
  for i = 1, #signals do
  local signal_name = signals[i][1]
  local signal_handler = signals[i][2]
  target.connect_signal(signal_name, signal_handler)
  end
end

local function connect_screen_signals(signals)
  _connect_signals(signals, screen)
end

local function connect_client_signals(signals)
  _connect_signals(signals, client)
end

return {
  screen_signals = screen_signals,
  connect_screen_signals = connect_screen_signals,
  client_signals = client_signals,
  connect_client_signals = connect_client_signals
}
