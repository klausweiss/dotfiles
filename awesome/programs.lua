return {
   terminal = "xterm",
   editor = "emacs",
   file_manager = "nemo",
   internet_browser = "firefox",
   launcher = "rofi -show run",
   mail_client = "thunderbird",
   screen_lock = "i3lock -c 000000",
   screenshooter = "flameshot gui",
   power_menu = "rofi -show power-menu -modi power-menu:~/.dotfiles/rofi/rofi-power-menu/rofi-power-menu",

   brightness_up = "light -A 4",
   brightness_down = "light -U 4",
   refresh_displays = "autorandr -c",
}
