local gears = require("gears")

local layouts_keys = require("keyboard.layouts")
local programs_keys = require("keyboard.programs")
local prompt_keys = require("keyboard.prompt")
local tags_keys = require("keyboard.tags")
local client_keys = require("keyboard.client")
local general_keys = require("keyboard.general")

-- {{{ Key bindings
local globalkeys = gears.table.join(
  layouts_keys.globalkeys,
  programs_keys.globalkeys,
  prompt_keys.globalkeys,
  tags_keys.globalkeys,
  general_keys.globalkeys
)

local clientkeys = client_keys.clientkeys


return {
  globalkeys = globalkeys,
  clientkeys = clientkeys
}
