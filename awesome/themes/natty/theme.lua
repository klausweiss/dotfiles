function this_script_dir()
   local str = debug.getinfo(2, "S").source:sub(2)
   return str:match("(.*/)")
end

local theme_assets = require("beautiful.theme_assets")
local dpi = require("beautiful.xresources").apply_dpi

local theme_path = this_script_dir()

local theme = {}

local red = "#ff1e1e"
local dark_red = "#721a1a"

local colors = {
   bg_primary   = "#363c4c",
   bg_secondary = "#222630",
   bg_ternary   = "#121419",
   bg_alert     = dark_red,

   fg_primary   = "#ffffff",
   fg_secondary = "#cccccc",
   fg_ternary = "#7c7c7c",
   fg_alert = red,

   border_primary   = "#535d6c",
   border_secondary = "#000000",
   border_alert     = red,
}

for key, color in pairs(colors) do
   theme[key] = color
end

theme.font          = "sans " .. dpi(14)

theme.bg_focus      = colors.bg_primary
theme.bg_normal     = colors.bg_secondary
theme.bg_minimize   = colors.bg_ternary
theme.bg_urgent     = colors.bg_alert
theme.bg_systray    = colors.bg_secondary

theme.fg_focus      = colors.fg_primary
theme.fg_normal     = colors.fg_secondary
theme.fg_urgent     = colors.fg_primary
theme.fg_minimize   = colors.fg_ternary

theme.border_focus  = colors.border_primary
theme.border_normal = colors.border_secondary
theme.border_marked = colors.border_alert


-- spacing
theme.useless_gap          = dpi(3)
theme.border_width         = dpi(1)
theme.systray_icon_spacing = dpi(5)

-- There are other variable sets
-- overriding the default one when
-- defined, the sets are:
-- taglist_[bg|fg]_[focus|urgent|occupied|empty|volatile]
-- tasklist_[bg|fg]_[focus|urgent]
-- titlebar_[bg|fg]_[normal|focus]
-- tooltip_[font|opacity|fg_color|bg_color|border_width|border_color]
-- mouse_finder_[color|timeout|animate_timeout|radius|factor]
-- prompt_[fg|bg|fg_cursor|bg_cursor|font]
-- hotkeys_[bg|fg|border_width|border_color|shape|opacity|modifiers_fg|label_bg|label_fg|group_margin|font|description_font]
-- Example:
--theme.taglist_bg_focus = "#ff0000"

-- Generate taglist squares:
local taglist_square_size = dpi(10)
theme.taglist_squares_sel = theme_assets.taglist_squares_sel(
   taglist_square_size, theme.fg_normal
)
theme.taglist_squares_unsel = theme_assets.taglist_squares_unsel(
   taglist_square_size, theme.fg_normal
)

-- Variables set for theming notifications:
-- notification_font
-- notification_[bg|fg]
-- notification_[width|height|margin]
-- notification_[border_color|border_width|shape|opacity]

-- Variables set for theming the menu:
-- menu_[bg|fg]_[normal|focus]
-- menu_[border_color|border_width]
theme.menu_submenu_icon = theme_path .. "images/submenu.png"
theme.menu_height = dpi(15)
theme.menu_width  = dpi(100)

-- You can add as many variables as
-- you wish and access them by using
-- beautiful.variable in your rc.lua
--theme.bg_widget = "#cc0000"

-- Define the image to load
theme.titlebar_close_button_normal = theme_path .. "images/titlebar-buttons/inactive.svg"
theme.titlebar_close_button_focus = theme_path .. "images/titlebar-buttons/close.svg"
theme.titlebar_close_button_focus_hover = theme_path .. "images/titlebar-buttons/close-hover.svg"

theme.titlebar_minimize_button_normal = theme_path .. "images/titlebar-buttons/inactive.svg"
theme.titlebar_minimize_button_focus = theme_path .. "images/titlebar-buttons/minimize.svg"
theme.titlebar_minimize_button_focus_hover = theme_path .. "images/titlebar-buttons/minimize-hover.svg"

theme.titlebar_ontop_button_normal_inactive = theme_path .. "images/titlebar-buttons/inactive.svg"
theme.titlebar_ontop_button_focus_inactive = theme_path .. "images/titlebar-buttons/ontop.svg"
theme.titlebar_ontop_button_focus_inactive_hover = theme_path .. "images/titlebar-buttons/ontop-hover.svg"
theme.titlebar_ontop_button_normal_active = theme_path .. "images/titlebar-buttons/inactive.svg"
theme.titlebar_ontop_button_focus_active = theme_path .. "images/titlebar-buttons/ontop-active.svg"
theme.titlebar_ontop_button_focus_active_hover = theme_path .. "images/titlebar-buttons/ontop-active-hover.svg"

theme.titlebar_sticky_button_normal_inactive = theme_path .. "images/titlebar-buttons/inactive.svg"
theme.titlebar_sticky_button_focus_inactive = theme_path .. "images/titlebar-buttons/sticky.svg"
theme.titlebar_sticky_button_focus_inactive_hover = theme_path .. "images/titlebar-buttons/sticky-hover.svg"
theme.titlebar_sticky_button_normal_active = theme_path .. "images/titlebar-buttons/inactive.svg"
theme.titlebar_sticky_button_focus_active = theme_path .. "images/titlebar-buttons/sticky-active.svg"
theme.titlebar_sticky_button_focus_active_hover = theme_path .. "images/titlebar-buttons/sticky-active-hover.svg"

theme.titlebar_floating_button_normal_inactive = theme_path .. "images/titlebar-buttons/inactive.svg"
theme.titlebar_floating_button_focus_inactive = theme_path .. "images/titlebar-buttons/floating.svg"
theme.titlebar_floating_button_focus_inactive_hover = theme_path .. "images/titlebar-buttons/floating-hover.svg"
theme.titlebar_floating_button_normal_active = theme_path .. "images/titlebar-buttons/inactive.svg"
theme.titlebar_floating_button_focus_active = theme_path .. "images/titlebar-buttons/floating-active.svg"
theme.titlebar_floating_button_focus_active_hover = theme_path .. "images/titlebar-buttons/floating-active-hover.svg"

theme.titlebar_maximized_button_normal_inactive = theme_path .. "images/titlebar-buttons/inactive.svg"
theme.titlebar_maximized_button_focus_inactive = theme_path .. "images/titlebar-buttons/maximize.svg"
theme.titlebar_maximized_button_focus_inactive_hover = theme_path .. "images/titlebar-buttons/maximize-hover.svg"
theme.titlebar_maximized_button_normal_active = theme_path .. "images/titlebar-buttons/inactive.svg"
theme.titlebar_maximized_button_focus_active = theme_path .. "images/titlebar-buttons/maximize-active.svg"
theme.titlebar_maximized_button_focus_active_hover = theme_path .. "images/titlebar-buttons/maximize-active-hover.svg"

theme.wallpaper = theme_path .. "images/backgrounds/dark.png"

-- You can use your own layout icons like this:
theme.layout_fairh = theme_path .. "images/layouts/fairhw.png"
theme.layout_fairv = theme_path .. "images/layouts/fairvw.png"
theme.layout_floating  = theme_path .. "images/layouts/floatingw.png"
theme.layout_magnifier = theme_path .. "images/layouts/magnifierw.png"
theme.layout_max = theme_path .. "images/layouts/maxw.png"
theme.layout_fullscreen = theme_path .. "images/layouts/fullscreenw.png"
theme.layout_tilebottom = theme_path .. "images/layouts/tilebottomw.png"
theme.layout_tileleft   = theme_path .. "images/layouts/tileleftw.png"
theme.layout_tile = theme_path .. "images/layouts/tilew.png"
theme.layout_tiletop = theme_path .. "images/layouts/tiletopw.png"
theme.layout_spiral  = theme_path .. "images/layouts/spiralw.png"
theme.layout_dwindle = theme_path .. "images/layouts/dwindlew.png"
theme.layout_cornernw = theme_path .. "images/layouts/cornernww.png"
theme.layout_cornerne = theme_path .. "images/layouts/cornernew.png"
theme.layout_cornersw = theme_path .. "images/layouts/cornersww.png"
theme.layout_cornerse = theme_path .. "images/layouts/cornersew.png"

-- Generate Awesome icon:
theme.awesome_icon = theme_assets.awesome_icon(
   theme.menu_height, theme.bg_focus, theme.fg_focus
)

-- Define the icon theme for application icons. If not set then the icons
-- from /usr/share/icons and /usr/share/icons/hicolor will be used.
theme.icon_theme = "Papirus"

return theme

-- vim: filetype=lua:expandtab:shiftwidth=4:tabstop=8:softtabstop=4:textwidth=80
