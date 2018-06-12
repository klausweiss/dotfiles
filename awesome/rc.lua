require("errors_handler")
require("globals")

local awful = require("awful")
local menubar = require("menubar")

local layouts = require("layouts")
local screens = require("screens")
local mouse = require("mouse")
local keyboard = require("keyboard.all")
local rules = require("rules")
local signals = require("signals")

require("awful.autofocus")
require("awful.hotkeys_popup.keys")


-- Keyboard
root.keys(keyboard.globalkeys)
-- Layouts
awful.layout.layouts = layouts
-- Mouse
root.buttons(mouse.rootbuttons)
-- Menu
menubar.utils.terminal = terminal
-- Rules
awful.rules.rules = rules
-- Signals
signals.connect_client_signals(signals.client_signals)
signals.connect_screen_signals(signals.screen_signals)
-- Screens
awful.screen.connect_for_each_screen(screens.for_each_screen)
