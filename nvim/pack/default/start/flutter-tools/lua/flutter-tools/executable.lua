local utils = require("flutter-tools.utils")
local path = require("flutter-tools.utils.path")
local ui = require("flutter-tools.ui")
---@type Job
local Job = require("plenary.job")

local fn = vim.fn
local luv = vim.loop

local M = {}

local dart_sdk = path.join("cache", "dart-sdk")

local function _flutter_sdk_root(bin_path)
  -- convert path/to/flutter/bin/flutter into path/to/flutter
  return fn.fnamemodify(bin_path, ":h:h")
end

local function _dart_sdk_root(paths)
  if paths.flutter_sdk then
    -- On Linux installations with snap the dart SDK can be further nested inside a bin directory
    -- so it's /bin/cache/dart-sdk whereas else where it is /cache/dart-sdk
    local segments = { paths.flutter_sdk, "cache" }
    if not path.is_dir(path.join(unpack(segments))) then table.insert(segments, 2, "bin") end
    if path.is_dir(path.join(unpack(segments))) then
      -- remove the /cache/ directory as it's already part of the SDK path above
      segments[#segments] = nil
      return path.join(unpack(vim.tbl_flatten({ segments, dart_sdk })))
    end
  end

  if utils.executable("flutter") then
    local flutter_path = fn.resolve(fn.exepath("flutter"))
    local flutter_bin = fn.fnamemodify(flutter_path, ":h")
    return path.join(flutter_bin, dart_sdk)
  end

  if utils.executable("dart") then return fn.resolve(fn.exepath("dart")) end

  return ""
end

local function _flutter_sdk_dart_bin(flutter_sdk)
  -- retrieve the Dart binary from the Flutter SDK
  local binary_name = require("flutter-tools.utils.path").is_windows and "dart.bat" or "dart"
  return path.join(flutter_sdk, "bin", binary_name)
end

---Get paths for flutter and dart based on the binary locations
---@return table<string, string>
local function get_default_binaries()
  local flutter_bin = fn.resolve(fn.exepath("flutter"))
  return {
    flutter_bin = flutter_bin,
    dart_bin = fn.resolve(fn.exepath("dart")),
    flutter_sdk = _flutter_sdk_root(flutter_bin),
  }
end

---@type table<string, string>
local _paths = nil

function M.reset_paths()
  _paths = nil
end

---Execute user's lookup command and pass it to the job callback
---@param lookup_cmd string
---@param callback fun(p: string, t: table<string, string>?)
---@return table<string, string>
local function path_from_lookup_cmd(lookup_cmd, callback)
  local paths = {}
  local parts = vim.split(lookup_cmd, " ")
  local cmd = parts[1]
  local args = vim.list_slice(parts, 2, #parts)

  local job = Job:new({ command = cmd, args = args })
  job:after_failure(vim.schedule_wrap(function()
    ui.notify(
      { string.format("Error running %s", lookup_cmd) },
      { timeout = 5000, level = ui.ERROR }
    )
  end))
  job:after_success(vim.schedule_wrap(function(j, _)
    local result = j:result()
    local flutter_sdk_path = result[1]
    if flutter_sdk_path then
      paths.dart_bin = _flutter_sdk_dart_bin(flutter_sdk_path)
      paths.flutter_bin = path.join(flutter_sdk_path, "bin", "flutter")
      paths.flutter_sdk = flutter_sdk_path
      callback(paths)
    else
      paths = get_default_binaries()
      callback(paths)
    end
    return paths
  end))
  job:start()
end

---Fetch the paths to the users binaries.
---@param callback fun(paths: table<string, string>)
---@return nil
function M.get(callback)
  local conf = require("flutter-tools.config").get()

  if _paths then return callback(_paths) end

  if conf.fvm then
    local flutter_bin_symlink = path.join(luv.cwd(), ".fvm", "flutter_sdk", "bin", "flutter")
    local flutter_bin = luv.fs_realpath(flutter_bin_symlink)
    if path.exists(flutter_bin_symlink) and path.exists(flutter_bin) then
      _paths = {
        flutter_bin = flutter_bin,
        flutter_sdk = _flutter_sdk_root(flutter_bin),
        fvm = true,
      }
      _paths.dart_sdk = _dart_sdk_root(_paths)
      _paths.dart_bin = _flutter_sdk_dart_bin(_paths.flutter_sdk)
      return callback(_paths)
    end
  end

  if conf.flutter_path then
    local flutter_path = fn.resolve(conf.flutter_path)
    _paths = {
      flutter_bin = flutter_path,
      flutter_sdk = _flutter_sdk_root(flutter_path),
    }
    _paths.dart_sdk = _dart_sdk_root(_paths)
    _paths.dart_bin = _flutter_sdk_dart_bin(_paths.flutter_sdk)
    return callback(_paths)
  end

  if conf.flutter_lookup_cmd then
    return path_from_lookup_cmd(conf.flutter_lookup_cmd, function(paths)
      _paths = paths
      _paths.dart_sdk = _dart_sdk_root(_paths)
      callback(_paths)
    end)
  end

  if not _paths then
    _paths = get_default_binaries()
    _paths.dart_sdk = _dart_sdk_root(_paths)
    if _paths.flutter_sdk then _paths.dart_bin = _flutter_sdk_dart_bin(_paths.flutter_sdk) end
  end

  return callback(_paths)
end

---Fetch the path to the users flutter installation.
---@param callback fun(paths: table<string, string>)
---@return nil
function M.flutter(callback)
  M.get(function(paths)
    callback(paths.flutter_bin)
  end)
end

return M
