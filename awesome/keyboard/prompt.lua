local gears = require("gears")
local awful = require("awful")
local menubar = require("menubar")

-- Prompt
local globalkeys = gears.table.join(
  awful.key({ capslock }, " ", function () awful.screen.focused().mypromptbox:run() end,
    {description = "run prompt", group = "launcher"}),

  -- awful.key({ modkey }, "x",
  --   function ()
  --   awful.prompt.run {
  --     prompt     = "Run Lua code: ",
  --     textbox    = awful.screen.focused().mypromptbox.widget,
  --     exe_callback = awful.util.eval,
  --     history_path = awful.util.get_cache_dir() .. "/history_eval"
  --   }
  --   end,
  --   {description = "lua execute prompt", group = "awesome"}),
  -- Menubar
  awful.key({ modkey }, "p", function() menubar.show() end,
    {description = "show the menubar", group = "launcher"})
)

return {
  globalkeys = globalkeys
}
