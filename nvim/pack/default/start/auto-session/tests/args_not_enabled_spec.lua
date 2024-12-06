---@diagnostic disable: undefined-field
local TL = require "tests/test_lib"
local stub = require "luassert.stub"

describe("The args not enabled config", function()
  local no_restore_hook_called = false
  require("auto-session").setup {
    args_allow_single_directory = false,
    args_allow_files_auto_save = false,

    -- Disable autosave so we leave our setup autosave for other tests
    auto_save_enabled = false,
    no_restore_cmds = {
      function()
        no_restore_hook_called = true
      end,
    },
  }
  TL.clearSessionFilesAndBuffers()

  it("can save a session", function()
    vim.cmd("e " .. TL.test_file)

    vim.cmd "SessionSave"

    -- Make sure the session was created
    assert.equals(1, vim.fn.filereadable(TL.default_session_path))

    -- Make sure the session has our buffer
    TL.assertSessionHasFile(TL.default_session_path, TL.test_file)

    -- now clear the buffers
    vim.cmd "%bw!"
  end)

  it("doesn't restore a session when run with a single directory", function()
    assert.equals(false, no_restore_hook_called)

    -- Stub
    local s = stub(vim.fn, "argv")
    s.returns { "." }

    -- only exported because we set the unit testing env in TL
    assert.equals(false, require("auto-session").auto_restore_session_at_vim_enter())

    -- Revert the stub
    vim.fn.argv:revert()

    assert.equals(true, no_restore_hook_called)

    assert.equals(0, vim.fn.bufexists(TL.test_file))
  end)

  it("doesn't restore a session when run with a file", function()
    no_restore_hook_called = false
    assert.equals(false, no_restore_hook_called)

    local s = stub(vim.fn, "argv")
    s.returns { TL.test_file }

    -- only exported because we set the unit testing env in TL
    assert.equals(false, require("auto-session").auto_restore_session_at_vim_enter())

    -- Revert the stub
    vim.fn.argv:revert()

    assert.equals(true, no_restore_hook_called)

    assert.equals(0, vim.fn.bufexists(TL.test_file))
  end)
end)
