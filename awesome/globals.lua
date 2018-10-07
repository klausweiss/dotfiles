local gears = require("gears")
local beautiful = require("beautiful")

-- Variable definitions
-- Themes define colours, icons, font and wallpapers.
theme = "dremora"
beautiful.init(gears.filesystem.get_configuration_dir() .. "themes/".. theme .. "/theme.lua")

-- This is used later as the default terminal and editor to run.
terminal = "xterm"
editor = os.getenv("EDITOR") or "vi"
editor_cmd = terminal .. " -e " .. editor
file_manager = "thunar"
internet_browser = "firefox"
mail_client = "thunderbird"

-- Default modkey.
-- Usually, Mod4 is the key with a logo between Control and Alt.
-- If you do not like this or do not have such a key,
-- I suggest you to remap Mod4 to another key using xmodmap or other tools.
-- However, you can use another modifier like Mod1, but it may interact with others.
super = "Mod4"
alt = "Mod1"
control = "Control"

modkey = alt
capslock = control
