local Lib = require "auto-session-library"
local autocmds = require "auto-session-autocmds"

-- Run comand hooks
local function run_hook_cmds(cmds, hook_name)
  local results = {}
  if not Lib.is_empty_table(cmds) then
    for _, cmd in ipairs(cmds) do
      Lib.logger.debug(string.format("Running %s command: %s", hook_name, cmd))
      local success, result

      if type(cmd) == "function" then
        success, result = pcall(cmd)
      else
        success, result = pcall(vim.cmd, cmd)
      end

      if not success then
        Lib.logger.error(string.format("Error running %s. error: %s", cmd, result))
      else
        table.insert(results, result)
      end
    end
  end
  return results
end

----------- Setup ----------
local AutoSession = {
  conf = {},
}

---table default config for auto session
---@class defaultConf
---@field log_level string debug, info, error
---@field auto_session_enable_last_session boolean
---@field auto_session_root_dir string root directory for session files, by default is `vim.fn.stdpath('data')/sessions/`
---@field auto_session_enabled boolean enable auto session
---@field auto_session_create_enabled boolean|nil Enables/disables auto creating new sessions
---@field auto_save_enabled? boolean Enables/disables auto saving session
---@field auto_restore_enabled? boolean Enables/disables auto restoring session
---@field auto_session_suppress_dirs? table Suppress auto session for directories
---@field auto_session_allowed_dirs? table Allow auto session for directories, if empty then all directories are allowed except for suppressed ones
---@field auto_session_use_git_branch boolean|nil Include git branch name in session name to differentiate between sessions for different git branches

---Default config for auto session
---@type defaultConf
local defaultConf = {
  log_level = vim.g.auto_session_log_level or AutoSession.conf.logLevel or AutoSession.conf.log_level or "info", -- Sets the log level of the plugin (debug, info, error). camelCase logLevel for compatibility.
  auto_session_enable_last_session = vim.g.auto_session_enable_last_session or false, -- Enables/disables the "last session" feature
  auto_session_root_dir = vim.fn.stdpath "data" .. "/sessions/", -- Root dir where sessions will be stored
  auto_session_enabled = true, -- Enables/disables auto creating, saving and restoring
  auto_session_create_enabled = nil, -- Enables/disables auto creating new sessions
  auto_save_enabled = nil, -- Enables/disables auto save feature
  auto_restore_enabled = nil, -- Enables/disables auto restore feature
  auto_session_suppress_dirs = nil, -- Suppress session restore/create in certain directories
  auto_session_allowed_dirs = nil, -- Allow session restore/create in certain directories
  auto_session_use_git_branch = vim.g.auto_session_use_git_branch or false,
}

---Lua Only Configs for Auto Session
---@class luaOnlyConf
---@field cwd_change_handling CwdChangeHandling
---@field bypass_session_save_file_types? table List of file types to bypass auto save when the only buffer open is one of the file types listed
local luaOnlyConf = {
  bypass_session_save_file_types = nil, -- Bypass auto save when only buffer open is one of these file types

  ---CWD Change Handling Config
  ---@class CwdChangeHandling
  ---@field restore_upcoming_session boolean {true} restore session for upcoming cwd on cwd change
  ---@field pre_cwd_changed_hook boolean? {true} This is called after auto_session code runs for the DirChangedPre autocmd
  ---@field post_cwd_changed_hook boolean? {true} This is called after auto_session code runs for the DirChanged autocmd

  ---@type CwdChangeHandling this config can also be set to `false` to disable cwd change handling altogether.
  --- Can also be set to a table with any of the following keys:
  ---  {
  ---    restore_upcoming_session = true,
  ---    pre_cwd_changed_hook = nil, -- lua function hook. This is called after auto_session code runs for the `DirChangedPre` autocmd
  ---    post_cwd_changed_hook = nil, -- lua function hook. This is called after auto_session code runs for the `DirChanged` autocmd
  ---  }
  ---@diagnostic disable-next-line: assign-type-mismatch
  cwd_change_handling = false, -- Config for handling the DirChangePre and DirChanged autocmds, can be set to false to disable altogether
}

-- Set default config on plugin load
AutoSession.conf = vim.tbl_deep_extend("force", defaultConf, luaOnlyConf)

-- Pass configs to Lib
Lib.conf = {
  log_level = AutoSession.conf.log_level,
}

---Setup function for AutoSession
---@param config defaultConf config for auto session
function AutoSession.setup(config)
  AutoSession.conf = vim.tbl_deep_extend("force", AutoSession.conf, config or {})
  Lib.ROOT_DIR = AutoSession.conf.auto_session_root_dir
  Lib.setup {
    log_level = AutoSession.conf.log_level,
  }

  autocmds.setup_autocmds(AutoSession.conf, AutoSession)
end

local function is_enabled()
  if vim.g.auto_session_enabled ~= nil then
    return vim.g.auto_session_enabled == Lib._VIM_TRUE
  elseif AutoSession.conf.auto_session_enabled ~= nil then
    return AutoSession.conf.auto_session_enabled
  end

  return true
end

local function is_allowed_dirs_enabled()
  Lib.logger.debug "==== is_allowed_dirs_enabled"
  if vim.g.auto_session_allowed_dirs ~= nil then
    return not vim.tbl_isempty(vim.g.auto_session_allowed_dirs)
  else
    return not vim.tbl_isempty(AutoSession.conf.auto_session_allowed_dirs or {})
  end

  return false
end

local function is_auto_create_enabled()
  if vim.g.auto_session_create_enabled ~= nil then
    return vim.g.auto_session_create_enabled == Lib._VIM_TRUE
  elseif AutoSession.conf.auto_session_create_enabled ~= nil then
    return AutoSession.conf.auto_session_create_enabled
  end

  return true
end

-- get the current git branch name, if any, and only if configured to do so
local function get_branch_name()
  if AutoSession.conf.auto_session_use_git_branch then
    local out = vim.fn.systemlist "git rev-parse --abbrev-ref HEAD"
    if vim.v.shell_error ~= 0 then
      Lib.logger.debug(string.format("git failed with: %s", table.concat(out, "\n")))
      return ""
    end
    return out[1]
  end

  return ""
end

local pager_mode = nil
local in_pager_mode = function()
  if pager_mode ~= nil then
    return pager_mode
  end -- Only evaluate this once

  local opened_with_args = next(vim.fn.argv()) ~= nil -- Neovim was opened with args
  local reading_from_stdin = vim.g.in_pager_mode == Lib._VIM_TRUE -- Set from StdinReadPre

  pager_mode = opened_with_args or reading_from_stdin
  Lib.logger.debug("==== Pager mode ", pager_mode)
  return pager_mode
end

local in_headless_mode = function()
  return not next(vim.api.nvim_list_uis())
end

local auto_save = function()
  if in_pager_mode() or in_headless_mode() then
    return false
  end

  if vim.g.auto_save_enabled ~= nil then
    return vim.g.auto_save_enabled == Lib._VIM_TRUE
  elseif AutoSession.conf.auto_save_enabled ~= nil then
    return AutoSession.conf.auto_save_enabled
  end

  return true
end

local auto_restore = function()
  if in_pager_mode() or in_headless_mode() then
    return false
  end

  if vim.g.auto_restore_enabled ~= nil then
    return vim.g.auto_restore_enabled == Lib._VIM_TRUE
  elseif AutoSession.conf.auto_restore_enabled ~= nil then
    return AutoSession.conf.auto_restore_enabled
  end

  return true
end

local function bypass_save_by_filetype()
  local file_types_to_bypass = AutoSession.conf.bypass_session_save_file_types or {}
  local windows = vim.api.nvim_list_wins()

  for _, current_window in ipairs(windows) do
    local buf = vim.api.nvim_win_get_buf(current_window)
    local buf_ft = vim.api.nvim_buf_get_option(buf, "filetype")

    local local_return = false
    for _, ft_to_bypass in ipairs(file_types_to_bypass) do
      if buf_ft == ft_to_bypass then
        local_return = true
        break
      end
    end

    if local_return == false then
      Lib.logger.debug "bypass_save_by_filetype: false"
      return false
    end
  end

  Lib.logger.debug "bypass_save_by_filetype: true"
  return true
end

local function suppress_session()
  local dirs = vim.g.auto_session_suppress_dirs or AutoSession.conf.auto_session_suppress_dirs or {}

  local cwd = vim.fn.getcwd()
  for _, s in pairs(dirs) do
    s = string.gsub(vim.fn.simplify(Lib.expand(s)), "/+$", "")
    for path in string.gmatch(s, "[^\r\n]+") do
      if cwd == path then
        return true
      end
    end
  end

  return false
end

local function is_allowed_dir()
  if not is_allowed_dirs_enabled() then
    return true
  end

  local dirs = vim.g.auto_session_allowed_dirs or AutoSession.conf.auto_session_allowed_dirs or {}
  local cwd = vim.fn.getcwd()
  for _, s in pairs(dirs) do
    s = string.gsub(vim.fn.simplify(Lib.expand(s)), "/+$", "")
    for path in string.gmatch(s, "[^\r\n]+") do
      if cwd == path then
        Lib.logger.debug("is_allowed_dir", true)
        return true
      end
    end
  end

  Lib.logger.debug("is_allowed_dir", false)
  return false
end

local function get_session_file_name(sessions_dir)
  local session = sessions_dir and sessions_dir ~= "" and sessions_dir or nil

  if Lib.is_empty(sessions_dir) then
    sessions_dir = AutoSession.get_root_dir()
  else
    sessions_dir = Lib.append_slash(sessions_dir)
  end

  if vim.fn.isdirectory(session or sessions_dir) == Lib._VIM_FALSE then
    -- When we get here session and sessions_dir either both point to a file or do not exist
    return session
  else
    local session_name = AutoSession.conf.auto_session_enable_last_session and Lib.conf.last_loaded_session
    if not session_name then
      session_name = Lib.escaped_session_name_from_cwd()
      local branch_name = get_branch_name()
      branch_name = Lib.escape_branch_name(branch_name ~= "" and "_" .. branch_name or "")
      session_name = string.format("%s%s", session_name, branch_name)
    end
    return string.format(sessions_dir .. "%s.vim", session_name)
  end
end

do
  ---Get latest session for the "last session" feature
  ---@return string|nil
  function AutoSession.get_latest_session()
    local dir = Lib.expand(AutoSession.conf.auto_session_root_dir)
    local latest_session = { session = nil, last_edited = 0 }

    for _, filename in ipairs(vim.fn.readdir(dir)) do
      local session = AutoSession.conf.auto_session_root_dir .. filename
      local last_edited = vim.fn.getftime(session)

      if last_edited > latest_session.last_edited then
        latest_session.session = session
        latest_session.last_edited = last_edited
      end
    end

    if latest_session.session ~= nil then
      -- Need to escape % chars on the filename so expansion doesn't happen
      return latest_session.session:gsub("%%", "\\%%")
    else
      return nil
    end
  end
end

local function auto_save_conditions_met()
  return is_enabled() and auto_save() and not suppress_session() and is_allowed_dir() and not bypass_save_by_filetype()
end

---AutoSaveSession
---Function called by auto_session to trigger auto_saving sessions, for example on VimExit events.
---@param sessions_dir? string the session directory to auto_save a session for. If empty this function will end up using the cwd to infer what session to save for.
function AutoSession.AutoSaveSession(sessions_dir)
  if auto_save_conditions_met() then
    if not is_auto_create_enabled() then
      local session_file_name = get_session_file_name(sessions_dir)
      if not Lib.is_readable(session_file_name) then
        return
      end
    end

    AutoSession.SaveSession(sessions_dir, true)
  end
end

---Gets the root directory of where to save the sessions.
---By default this resolves to `vim.fn.stdpath "data" .. "/sessions/"`
---@return string
function AutoSession.get_root_dir()
  if AutoSession.validated then
    return AutoSession.conf.auto_session_root_dir
  end

  local root_dir = vim.g["auto_session_root_dir"] or AutoSession.conf.auto_session_root_dir
  Lib.init_dir(root_dir)

  AutoSession.conf.auto_session_root_dir = Lib.validate_root_dir(root_dir)
  AutoSession.validated = true
  return root_dir
end

---Get the hook commands to run
---This function gets cmds from both lua and vimscript configs
---@param typ string
---@return function[]|string[]
function AutoSession.get_cmds(typ)
  return AutoSession.conf[typ .. "_cmds"] or vim.g["auto_session_" .. typ .. "_cmds"]
end

local function message_after_saving(path, auto)
  if auto then
    Lib.logger.debug("Session saved at " .. path)
  else
    Lib.logger.info("Session saved at " .. path)
  end
end

--Save extra info to "{session_file}x.vim"
local function save_extra_cmds(session_file_name)
  local extra_cmds = AutoSession.get_cmds "save_extra"

  if extra_cmds then
    local datas = run_hook_cmds(extra_cmds, "save-extra")
    local extra_file = string.gsub(session_file_name, ".vim$", "x.vim")
    extra_file = string.gsub(extra_file, "\\%%", "%%")
    vim.fn.writefile(datas, extra_file)
  end
end

---@class PickerItem
---@field display_name string
---@field path string

--- Formats an autosession file name to be more presentable to a user
---@param path string
---@return string
function AutoSession.format_file_name(path)
  return Lib.unescape_dir(path):match "(.+)%.vim"
end

---@return PickerItem[]
function AutoSession.get_session_files()
  local files = {}
  local sessions_dir = AutoSession.get_root_dir()

  if not vim.fn.isdirectory(sessions_dir) then
    return files
  end

  local entries = vim.fn.readdir(sessions_dir, function(item)
    return vim.fn.isdirectory(item) == 0 and not string.find(item, "x.vim$")
  end)

  return vim.tbl_map(function(entry)
    return { display_name = AutoSession.format_file_name(entry), path = entry }
  end, entries)
end

---@param files string[]
---@param prompt string
---@param callback fun(choice: PickerItem)
local function open_picker(files, prompt, callback)
  vim.ui.select(files, {
    prompt = prompt,
    kind = "auto-session",
    format_item = function(item)
      return item.display_name
    end,
  }, function(choice)
    if choice then
      callback(choice)
    end
  end)
end

---@param data table
local function handle_autosession_command(data)
  local files = AutoSession.get_session_files()
  if data.args:match "search" then
    open_picker(files, "Select a session:", function(choice)
      -- Change dir to selected session path, the DirChangePre and DirChange events will take care of the rest
      vim.fn.chdir(choice.display_name)
    end)
  elseif data.args:match "delete" then
    open_picker(files, "Delete a session:", function(choice)
      AutoSession.DeleteSessionByName(choice.display_name)
    end)
  end
end

vim.api.nvim_create_user_command("Autosession", handle_autosession_command, { nargs = 1 })

--Saves the session, overriding if previously existing.
---@param sessions_dir? string
---@param auto boolean
function AutoSession.SaveSession(sessions_dir, auto)
  Lib.logger.debug "==== SaveSession"
  local session_file_name = get_session_file_name(sessions_dir)

  local pre_cmds = AutoSession.get_cmds "pre_save"
  run_hook_cmds(pre_cmds, "pre-save")

  vim.cmd("mks! " .. session_file_name)

  save_extra_cmds(session_file_name)
  message_after_saving(session_file_name, auto)

  local post_cmds = AutoSession.get_cmds "post_save"
  run_hook_cmds(post_cmds, "post-save")
end

---Function called by AutoSession when automatically restoring a session.
---This function avoids calling RestoreSession automatically when argv is not nil.
---@param sessions_dir any
---@return boolean boolean returns whether restoring the session was successful or not.
function AutoSession.AutoRestoreSession(sessions_dir)
  if is_enabled() and auto_restore() and not suppress_session() then
    return AutoSession.RestoreSession(sessions_dir)
  end

  return false
end

local function extract_dir_or_file(sessions_dir_or_file)
  local sessions_dir = nil
  local session_file = nil

  if Lib.is_empty(sessions_dir_or_file) then
    sessions_dir = AutoSession.get_root_dir()
  elseif vim.fn.isdirectory(Lib.expand(sessions_dir_or_file)) == Lib._VIM_TRUE then
    if not Lib.ends_with(sessions_dir_or_file, "/") then
      sessions_dir = Lib.append_slash(sessions_dir_or_file)
    else
      sessions_dir = sessions_dir_or_file
    end
  else
    session_file = sessions_dir_or_file
  end

  return sessions_dir, session_file
end

---RestoreSessionFromFile takes a session_file and calls RestoreSession after parsing the provided parameter.
---@param session_file string
function AutoSession.RestoreSessionFromFile(session_file)
  AutoSession.RestoreSession(string.format(AutoSession.get_root_dir() .. "%s.vim", session_file:gsub("/", "%%")))
end

-- TODO: make this more readable!
---Restores the session by sourcing the session file if it exists/is readable.
---This function is intended to be called by the user but it is also called by `AutoRestoreSession`
---@param sessions_dir_or_file string a dir string or a file string
---@return boolean boolean returns whether restoring the session was successful or not.
function AutoSession.RestoreSession(sessions_dir_or_file)
  local sessions_dir, session_file = extract_dir_or_file(sessions_dir_or_file)
  Lib.logger.debug("sessions_dir, session_file", sessions_dir, session_file)

  local restore = function(file_path, session_name)
    local pre_cmds = AutoSession.get_cmds "pre_restore"
    run_hook_cmds(pre_cmds, "pre-restore")

    local cmd = "silent source " .. file_path
    local success, result = pcall(vim.cmd, cmd)

    if not success then
      Lib.logger.error([[
        Error restoring session! The session might be corrupted.
        Disabling auto save. Please check for errors in your config. Error:
      ]] .. result)
      AutoSession.conf.auto_save_enabled = false
      return
    end

    Lib.logger.info("Session restored from " .. file_path)

    if AutoSession.conf.auto_session_enable_last_session then
      Lib.conf.last_loaded_session = session_name
    end

    local post_cmds = AutoSession.get_cmds "post_restore"
    run_hook_cmds(post_cmds, "post-restore")
  end

  -- I still don't like reading this chunk, please cleanup
  if sessions_dir then
    Lib.logger.debug "==== Using session DIR"

    local session_name = AutoSession.conf.auto_session_enable_last_session and Lib.conf.last_loaded_session
    local session_file_path
    if not session_name then
      session_file_path = get_session_file_name(sessions_dir)
      session_name = vim.fn.fnamemodify(session_file_path, ":t:r")
    else
      session_file_path = string.format(sessions_dir .. "%s.vim", session_name)
    end
    Lib.logger.debug("==== Session Name:", session_name)

    local legacy_session_name = Lib.legacy_session_name_from_cwd()
    local legacy_file_path = string.format(sessions_dir .. "%s.vim", legacy_session_name)

    if Lib.is_readable(session_file_path) then
      restore(session_file_path, session_name)
    elseif Lib.is_readable(legacy_file_path) then
      restore(legacy_file_path, session_name)
    else
      if AutoSession.conf.auto_session_enable_last_session then
        local last_session_file_path = AutoSession.get_latest_session()
        if last_session_file_path ~= nil then
          Lib.logger.info("Restoring last session:", last_session_file_path)
          restore(last_session_file_path)
        end
      else
        Lib.logger.debug "File not readable, not restoring session"
        return false
      end
    end
  elseif session_file then
    Lib.logger.debug "==== Using session FILE"
    local escaped_file = session_file:gsub("%%", "\\%%")
    if Lib.is_readable(escaped_file) then
      Lib.logger.debug "isReadable, calling restore"
      restore(escaped_file)
    else
      Lib.logger.debug "File not readable, not restoring session"
    end
  else
    Lib.logger.error "Error while trying to parse session dir or file"
  end

  return true
end

local maybe_disable_autosave = function(session_name)
  local current_session = Lib.escaped_session_name_from_cwd()

  if session_name == current_session then
    Lib.logger.debug(
      "Auto Save disabled for current session.",
      vim.inspect {
        session_name = session_name,
        current_session = current_session,
      }
    )
    AutoSession.conf.auto_save_enabled = false
  else
    Lib.logger.debug(
      "Auto Save is still enabled for current session.",
      vim.inspect {
        session_name = session_name,
        current_session = current_session,
      }
    )
  end
end

---DisableAutoSave
---Intended to be called by the user
function AutoSession.DisableAutoSave()
  Lib.logger.debug "Auto Save disabled manually."
  AutoSession.conf.auto_save_enabled = false
end

---CompleteSessions is used by the vimscript command for session name/path completion.
---@return string
function AutoSession.CompleteSessions()
  local session_files = vim.fn.glob(AutoSession.get_root_dir() .. "/*", true, { true })
  local session_names = {}

  for _, sf in ipairs(session_files) do
    local name = Lib.unescape_dir(vim.fn.fnamemodify(sf, ":t:r"))
    table.insert(session_names, name)
  end

  return table.concat(session_names, "\n")
end

---DeleteSessionByName deletes sessions given a provided list of paths
---@param ... string[]
function AutoSession.DeleteSessionByName(...)
  local session_paths = {}

  for _, name in ipairs { ... } do
    local escaped_session = Lib.escape_dir(name)
    maybe_disable_autosave(escaped_session)
    local session_path = string.format("%s/%s.vim", AutoSession.get_root_dir(), escaped_session)
    Lib.logger.debug("Deleting session", session_path)
    table.insert(session_paths, session_path)
  end
  AutoSession.DeleteSession(unpack(session_paths))
end

---DeleteSession delets a single session given a provided path
---@param ... string[]
function AutoSession.DeleteSession(...)
  local pre_cmds = AutoSession.get_cmds "pre_delete"
  run_hook_cmds(pre_cmds, "pre-delete")

  -- TODO: make the delete command customizable
  local is_win32 = vim.fn.has "win32" == Lib._VIM_TRUE

  if not Lib.is_empty(...) then
    for _, file_path in ipairs { ... } do
      Lib.logger.debug("session_file_path", file_path)

      vim.fn.delete(Lib.expand(file_path))

      Lib.logger.info("Deleted session " .. file_path)
    end
  else
    local session_name = Lib.escaped_session_name_from_cwd()
    Lib.logger.debug("session_name", session_name)
    if is_win32 then
      session_name = session_name:gsub("\\", "")
    end

    local session_file_path = string.format(AutoSession.get_root_dir() .. "%s.vim", session_name)
    vim.fn.delete(Lib.expand(session_file_path))

    maybe_disable_autosave(session_name)
    Lib.logger.info("Deleted session " .. session_file_path)
  end

  local post_cmds = AutoSession.get_cmds "post_delete"
  run_hook_cmds(post_cmds, "post-delete")
end

return AutoSession
