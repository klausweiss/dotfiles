---@diagnostic disable: undefined-field, undefined-global
-- Define helper aliases
local new_set = MiniTest.new_set
local expect, eq = MiniTest.expect, MiniTest.expect.equality

-- Create (but not start) child Neovim object
local child = MiniTest.new_child_neovim()

local TL = require "tests/test_lib"

-- Define main test set of this file
local T = new_set {
  -- Register hooks
  hooks = {
    pre_once = function()
      TL.clearSessionFilesAndBuffers()
    end,
    -- This will be executed before every (even nested) case
    pre_case = function()
      -- Restart child process with custom 'init.lua' script
      child.restart { "-u", "scripts/minimal_init_mini.lua" }
      -- Load tested plugin
      child.lua [[M = require('auto-session').setup({
        auto_save_enabled = false,
        auto_restore_enabled = false,
      })]]
    end,
    -- This will be executed one after all tests from this set are finished
    post_once = child.stop,
  },
}

T["session lens"] = new_set {}

T["session lens"]["save a default session"] = function()
  child.cmd("e " .. TL.test_file)
  expect.equality(1, child.fn.bufexists(TL.test_file))
  child.cmd "SessionSave"

  expect.equality(1, child.fn.bufexists(TL.test_file))
  expect.equality(1, vim.fn.filereadable(TL.default_session_path))
end

T["session lens"]["save a named session"] = function()
  child.cmd("e " .. TL.test_file)
  expect.equality(1, child.fn.bufexists(TL.test_file))
  child.cmd("SessionSave " .. TL.named_session_name)
  expect.equality(1, vim.fn.filereadable(TL.named_session_path))

  child.cmd("e " .. TL.other_file)
  child.cmd "SessionSave project_x"
end

T["session lens"]["can load a session"] = function()
  expect.equality(0, child.fn.bufexists(TL.test_file))
  child.cmd "SessionSearch"
  -- give the UI time to come up
  vim.loop.sleep(250)
  child.type_keys "project_x"
  child.type_keys "<cr>"
  -- give the session time to load
  vim.loop.sleep(500)
  expect.equality(1, child.fn.bufexists(TL.other_file))
end

T["session lens"]["can copy a session"] = function()
  expect.equality(0, child.fn.bufexists(TL.test_file))
  child.cmd "SessionSearch"
  -- give the UI time to come up
  local session_name = "project_x"
  vim.loop.sleep(250)
  child.type_keys(session_name)
  vim.loop.sleep(20)
  child.type_keys "<C-Y>"
  vim.loop.sleep(20)

  -- will append to session_name
  local copy_name = "copy"
  child.type_keys(copy_name .. "<cr>")
  -- give the session time to load
  vim.loop.sleep(500)
  expect.equality(1, vim.fn.filereadable(TL.makeSessionPath(session_name .. copy_name)))
end

T["session lens"]["can delete a session"] = function()
  expect.equality(1, vim.fn.filereadable(TL.named_session_path))
  child.cmd "SessionSearch"
  -- give the UI time to come up
  vim.loop.sleep(250)
  child.type_keys "mysession"
  child.type_keys "<c-d>"
  vim.loop.sleep(250)
  expect.equality(0, vim.fn.filereadable(TL.named_session_path))
end

-- Return test set which will be collected and execute inside `MiniTest.run()`
return T
