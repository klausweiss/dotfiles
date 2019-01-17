local ez = require("ez")

ez.theme.gaps = 3
ez.theme.wallpaper = "~/.config/wallpaper"

ez.layout.layouts = {
   tile,
   max,
   floating,
} 

local tag_web  = " web"
local tag_dev  = " dev"
local tag_term = " term"
local tag_im   = " im"
local tag_5    = " 5"
local tag_6    = " 6"
local tag_7    = " 7"

ez.tags.tags = {tag_web, tag_dev, tag_term, tag_im, tag_5, tag_6, tag_7}

ez.mouse.desktop_wheel_down = show_prev_tag
ez.mouse.desktop_wheel_up   = show_next_tag
ez.mouse.client_click              = focus_client
ez.mouse.client_click[{alt}]       = move_client
ez.mouse.client_right_click[{alt}] = resize_client

-- layouts
ez.keyboard.global[{alt,        " "}] = next_layout
ez.keyboard.global[{alt, shift, " "}] = prev_layout

-- windows
ez.keyboard.global[{alt,        tab}]   = focus_next_client
ez.keyboard.global[{alt, shift, tab}]   = focus_prev_client
ez.keyboard.client[{alt, shift, "f"}]   = toggle_fullscreen_client
ez.keyboard.client[{alt, shift, "m"}]   = toggle_maximize_client
ez.keyboard.client[{alt, "w"}]          = close_client
ez.keyboard.client[{alt, shift, enter}] = select_main_client

-- awesome
ez.keyboard.global[{alt, shift, "r"}]  = awesome.restart
ez.keyboard.global[{alt, shift, "q"}]  = awesome.quit

-- launcher
local launcher = "rofi -show run"
ez.keyboard.global[{ctrl,  " "}] = run(launcher)
ez.keyboard.global[{super, " "}] = require("menubar").show

-- multiple monitors
ez.keyboard.global[{alt, "m"}] = focus_left_screen
ez.keyboard.global[{alt, ","}] = move_client_to_next_screen
ez.keyboard.global[{alt, "."}] = focus_right_screen

-- tags
ez.keyboard.tags[{     ctrl       }] = show_tag_by_index
ez.keyboard.tags[{alt, ctrl       }] = toggle_tag_by_index
ez.keyboard.tags[{     ctrl, shift}] = move_focused_client_to_tag
ez.keyboard.tags[{alt, ctrl, shift}] = toggle_tag_on_focused_client

-- programs
local terminal         = "xterm"
local file_manager     = "thunar"
local internet_browser = "firefox"
local screen_lock      = "slock"
ez.keyboard.global[{alt, ctrl, enter}] = run(terminal)
ez.keyboard.global[{alt, shift, "e"}]  = run(file_manager)
ez.keyboard.global[{alt, shift, "b"}]  = run(internet_browser)
ez.keyboard.global[{alt, shift, "l"}]  = run(screen_lock)

-- system
ez.keyboard.global[{"XF86MonBrightnessUp"}]   = run("light -A 3")
ez.keyboard.global[{"XF86MonBrightnessDown"}] = run("light -U 3")
ez.keyboard.global[{"XF86AudioRaiseVolume"}]  = run("amixer sset Master 4%+")
ez.keyboard.global[{"XF86AudioLowerVolume"}]  = run("amixer sset Master 4%-")

ez.wibar.left = {
   launcher_menu,
   taglist,
}

ez.wibar.middle = {
   tasklist,
}

ez.wibar.right = {
   require("widgets.volumebar"),
   require("widgets.battery-widget"),
   tray,
   clock,
   layouts_switcher,
}

ez.client.focus_follow_mouse = true

ez.rules["Emacs"]      .tag = tag_dev
ez.rules["Firefox"]    .tag = tag_web
ez.rules["Fractal"]    .tag = tag_im
ez.rules["Telegram"]   .tag = tag_im
ez.rules["Thunderbird"].tag = tag_im
