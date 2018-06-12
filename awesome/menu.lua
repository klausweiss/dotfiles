local awful = require("awful")
local beautiful = require("beautiful")
local hotkeys_popup = require("awful.hotkeys_popup").widget

-- {{{ Helper functions
local function client_menu_toggle_fn()
  local instance = nil

  return function ()
    if instance and instance.wibox.visible then
      instance:hide()
      instance = nil
    else
      instance = awful.menu.clients({ theme = { width = 250 } })
    end
  end
end
-- }}}

-- Menu
-- Create a launcher widget and a main menu
local awesomemenu = {
   { "hotkeys", function() return false, hotkeys_popup.show_help end},
   { "manual", terminal .. " -e man awesome" },
   { "edit config", editor_cmd .. " " .. awesome.conffile },
   { "restart", awesome.restart },
   { "quit", function() awesome.quit() end}
}

local mainmenu = awful.menu({ items = { { "awesome", awesomemenu, beautiful.awesome_icon },
                    { "open terminal", terminal }
                    }
              })

local launcher = awful.widget.launcher({ image = beautiful.awesome_icon,
                     menu = mainmenu })

return {
  launcher = launcher,
  mainmenu = mainmenu,
  awesomemenu = awesomemenu,
  client_menu_toggle_fn = client_menu_toggle_fn
}
