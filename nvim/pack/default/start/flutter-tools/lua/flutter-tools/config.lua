local path = require("flutter-tools.utils.path")
local M = {}

local fn = vim.fn
local fmt = string.format

--- @param prefs table user preferences
local function validate_prefs(prefs)
  if prefs.flutter_path and prefs.flutter_lookup_cmd then
    vim.schedule(function()
      vim.notify(
        'Only one of "flutter_path" and "flutter_lookup_cmd" are required. Please remove one of the keys',
        vim.log.levels.ERROR
      )
    end)
  end
  vim.validate({
    outline = { prefs.outline, "table", true },
    dev_log = { prefs.dev_log, "table", true },
    closing_tags = { prefs.closing_tags, "table", true },
  })
end

---Create a proportional split using a percentage specified as a float
---@param percentage number
---@param fallback number
---@return string
local function get_split_cmd(percentage, fallback)
  return string.format("botright %dvnew", math.max(vim.o.columns * percentage, fallback))
end

local function get_default_lookup()
  local exepath = fn.exepath("flutter")
  local is_snap_installation = exepath and exepath:match("snap") or false
  return (path.is_linux and is_snap_installation) and "flutter sdk-path" or nil
end

M.debug_levels = {
  DEBUG = 1,
  WARN = 2,
}

local config = {
  flutter_path = nil,
  flutter_lookup_cmd = get_default_lookup(),
  fvm = false,
  widget_guides = {
    enabled = false,
    debug = false,
  },
  ui = setmetatable({
    border = "single",
  }, {
    __index = function(_, k)
      if k == "notification_style" then
        local ok = pcall(require, "notify")
        return ok and "native" or "plugin"
      end
    end,
  }),
  decorations = {
    statusline = {
      app_version = false,
      device = false,
    },
  },
  debugger = {
    enabled = false,
    run_via_dap = false,
    exception_breakpoints = nil,
    register_configurations = function(paths)
      require("dap").configurations.dart = {
        {
          type = "dart",
          request = "launch",
          name = "Launch flutter",
          dartSdkPath = paths.dart_sdk,
          flutterSdkPath = paths.flutter_sdk,
          program = "${workspaceFolder}/lib/main.dart",
          cwd = "${workspaceFolder}",
        },
        {
          type = "dart",
          request = "attach",
          name = "Connect flutter",
          dartSdkPath = paths.dart_sdk,
          flutterSdkPath = paths.flutter_sdk,
          program = "${workspaceFolder}/lib/main.dart",
          cwd = "${workspaceFolder}",
        },
      }
    end,
  },
  closing_tags = {
    highlight = "Comment",
    prefix = "// ",
    enabled = true,
  },
  lsp = {
    debug = M.debug_levels.WARN,
    color = {
      enabled = false,
      background = false,
      foreground = false,
      virtual_text = true,
      virtual_text_str = "■",
      background_color = nil,
    },
  },
  outline = setmetatable({
    auto_open = false,
  }, {
    __index = function(_, k)
      return k == "open_cmd" and get_split_cmd(0.3, 40) or nil
    end,
  }),
  dev_log = setmetatable({
    enabled = true,
  }, {
    __index = function(_, k)
      return k == "open_cmd" and get_split_cmd(0.4, 50) or nil
    end,
  }),
  dev_tools = {
    autostart = false,
    auto_open_browser = false,
  },
}

local deprecations = {
  flutter_outline = {
    fallback = "widget_guides",
    message = "please use 'widget_guides' instead",
  },
}

local function handle_deprecation(key, value, conf)
  local deprecation = deprecations[key]
  if not deprecation then return end
  vim.defer_fn(function()
    local ui = require("flutter-tools.ui")
    ui.notify(fmt("%s is deprecated: %s", key, deprecation.message), ui.WARN)
  end, 1000)
  if deprecation.fallback then conf[deprecation.fallback] = value end
end

---Get the configuration or just a key of the config
---@param key string?
---@return any
function M.get(key)
  if key then return config[key] end
  return config
end

function M.set(user_config)
  if not user_config or type(user_config) ~= "table" then return config end
  validate_prefs(user_config)
  for key, value in pairs(user_config) do
    handle_deprecation(key, value, user_config)
  end
  config = require("flutter-tools.utils").merge(config, user_config)
  return config
end

return M
