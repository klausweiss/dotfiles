local ez = require("ez")

ez.theme = "natty"
ez.theme.gaps = 0
ez.theme.wallpaper = "~/.config/wallpaper"

ez.layouts = {
   tile,
   max,
   magnifier,
   floating,
} 

local tag_web = " web"
local tag_dev = " dev"
local tag_term = " term"
local tag_im = " im"
local tag_5 = " 5"

ez.tags = {tag_web, tag_dev, tag_term, tag_im, tag_5, tag_6, tag_7}

ez.mouse.desktop_wheel_down = show_next_tag
ez.mouse.desktop_wheel_up = show_prev_tag
ez.mouse.client_click = focus_client
ez.mouse.client_click[{alt}] = move_client
ez.mouse.client_right_click[{alt}] = resize_client

-- layouts
ez.keyboard.global[{alt, " "}] = next_layout
ez.keyboard.global[{alt, shift, " "}] = prev_layout

-- windows
ez.keyboard.global[{alt, tab}] = focus_next_client
ez.keyboard.global[{alt, shift, tab}] = focus_prev_client
ez.keyboard.global[{alt, ctrl, "i"}] = focus_up_client
ez.keyboard.global[{alt, ctrl, "l"}] = focus_right_client
ez.keyboard.global[{alt, ctrl, "k"}] = focus_down_client
ez.keyboard.global[{alt, ctrl, "j"}] = focus_left_client
ez.keyboard.client[{alt, shift, "z"}] = toggle_fullscreen_client
ez.keyboard.client[{alt, shift, "m"}] = toggle_maximize_client
ez.keyboard.client[{alt, "w"}] = close_client
ez.keyboard.client[{alt, shift, enter}] = select_main_client

-- awesome
ez.keyboard.global[{alt, shift, "r"}]  = awesome.restart
ez.keyboard.global[{alt, shift, "q"}]  = awesome.quit

-- launcher
local launcher = "rofi -show run"
ez.keyboard.global[{ctrl, shift, " "}] = run(launcher)
ez.keyboard.global[{super, " "}] = require("menubar").show

-- multiple monitors
ez.keyboard.global[{alt, "d"}] = focus_left_screen
ez.keyboard.global[{alt, "k"}] = move_client_to_next_screen
ez.keyboard.global[{alt, "g"}] = focus_right_screen

-- tags
ez.keyboard.tags[{ctrl}] = show_tag_by_index
ez.keyboard.tags[{alt, ctrl}] = toggle_tag_by_index
ez.keyboard.tags[{ctrl, shift}] = move_focused_client_to_tag
ez.keyboard.tags[{alt, ctrl, shift}] = toggle_tag_on_focused_client

-- programs
local terminal = "xterm"
local file_manager = "thunar"
local internet_browser = "firefox"
local mail_client = "thunderbird"
local screen_lock = "slock"
ez.keyboard.global[{alt, ctrl, enter}] = run(terminal)
ez.keyboard.global[{alt, shift, "e"}] = run(file_manager)
ez.keyboard.global[{alt, shift, "w"}] = run(internet_browser)
ez.keyboard.global[{alt, shift, "l"}] = run(screen_lock)
ez.keyboard.global[{alt, shift, "x"}] = run("emacs")

-- system
local screenshooter = "flameshot gui"
ez.keyboard.global[{"Print"}] = run(screenshooter)
ez.keyboard.global[{"XF86MonBrightnessUp"}] = run("light -A 4")
ez.keyboard.global[{"XF86MonBrightnessDown"}] = run("light -U 4")
ez.keyboard.global[{"XF86AudioRaiseVolume"}] = run("pactl set-sink-volume @DEFAULT_SINK@ +5%")
ez.keyboard.global[{"XF86AudioLowerVolume"}] = run("pactl set-sink-volume @DEFAULT_SINK@ -5%")
ez.keyboard.global[{"XF86AudioMute"}] = run("pactl set-sink-mute @DEFAULT_SINK@ toggle")
ez.keyboard.global[{shift, "XF86AudioMute"}] = run("pactl set-source-mute @DEFAULT_SOURCE@ toggle")
ez.keyboard.global[{"XF86Launch5"}] = run("pactl set-sink-port 0 analog-output-lineout")
ez.keyboard.global[{"XF86Launch6"}] = run("pactl set-sink-port 0 analog-output-speaker")
ez.keyboard.global[{"XF86Tools"}] = run("autorandr -c")

ez.wibar.left = {
   launcher_menu,
   taglist,
}

ez.wibar.middle = {
   tasklist,
}

ez.wibar.right = {
   function (_screen)
      return common.mkwidget(common.texticon_box("", require("obvious.volume_alsa")()))
   end,
   function (_screen)
      return common.mkwidget(common.texticon_box("", require("obvious.battery")()))
   end,
   tray,
   date,
   time,
   layouts_switcher,
}

ez.client.focus_follow_mouse = true
ez.client.titlebar.left = {}
ez.client.titlebar.middle = {}
ez.client.titlebar.right = {
   floating, ontop, sticky,
   minimize, maximize, close,
}

ez.rules["Emacs"].tag = tag_dev
ez.rules["Evolution"].tag = tag_im
ez.rules["Firefox"].tag = tag_web
ez.rules["Fractal"].tag = tag_im
ez.rules["Telegram"].tag = tag_im
ez.rules["Thunderbird"].tag = tag_im

ez.rules["jetbrains-idea-ce"].tag = tag_dev
ez.rules["jetbrains-pycharm-ce"].tag = tag_dev
ez.rules["jetbrains-studio"].tag = tag_dev
