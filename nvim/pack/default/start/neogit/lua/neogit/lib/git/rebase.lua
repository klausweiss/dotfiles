local logger = require("neogit.logger")
local client = require("neogit.client")
local notif = require("neogit.lib.notification")

local M = {}

local a = require("plenary.async")

local function rebase_command(cmd)
  local git = require("neogit.lib.git")
  cmd = cmd or git.cli.rebase
  local envs = client.get_envs_git_editor()
  return cmd.env(envs).show_popup(true):in_pty(true).call(true)
end

function M.rebase_interactive(...)
  a.util.scheduler()
  local git = require("neogit.lib.git")
  local result = rebase_command(git.cli.rebase.interactive.args(...))
  if result.code ~= 0 then
    notif.create("Rebasing failed. Resolve conflicts before continuing", vim.log.levels.ERROR)
  end
  a.util.scheduler()
  local status = require("neogit.status")
  status.refresh(true, "rebase_interactive")
end

function M.rebase_onto(branch, args)
  a.util.scheduler()
  local git = require("neogit.lib.git")
  local result = rebase_command(git.cli.rebase.args(branch).arg_list(args))
  if result.code ~= 0 then
    notif.create("Rebasing failed. Resolve conflicts before continuing", vim.log.levels.ERROR)
  end
end

function M.continue()
  local git = require("neogit.lib.git")
  return rebase_command(git.cli.rebase.continue)
end

function M.skip()
  local git = require("neogit.lib.git")
  return rebase_command(git.cli.rebase.skip)
end

local uv = require("neogit.lib.uv")
function M.update_rebase_status(state)
  local cli = require("neogit.lib.git.cli")
  local root = cli.git_root()
  if root == "" then
    return
  end

  local rebase = {
    items = {},
    head = nil,
  }

  local _, stat = a.uv.fs_stat(root .. "/.git/rebase-merge")
  local rebase_file = nil

  -- Find the rebase progress files
  if stat then
    rebase_file = root .. "/.git/rebase-merge"
  else
    local _, stat = a.uv.fs_stat(root .. "/.git/rebase-apply")
    if stat then
      rebase_file = root .. "/.git/rebase-apply"
    end
  end

  if rebase_file then
    local err, head = uv.read_file(rebase_file .. "/head-name")
    if not head then
      logger.error("Failed to read rebase-merge head: " .. err)
      return
    end
    head = head:match("refs/heads/([^\r\n]+)")
    rebase.head = head

    local _, todos = uv.read_file(rebase_file .. "/git-rebase-todo")
    local _, done = uv.read_file(rebase_file .. "/done")

    local current = 0
    -- we need \r? to support windows
    for line in (done or ""):gmatch("[^\r\n]+") do
      if not line:match("^#") then
        current = current + 1
        table.insert(rebase.items, { name = line, done = true })
      end
    end

    rebase.current = current

    local cur = rebase.items[#rebase.items]
    if cur then
      cur.done = false
      cur.stopped = true
    end

    for line in (todos or ""):gmatch("[^\r\n]+") do
      if not line:match("^#") then
        table.insert(rebase.items, { name = line })
      end
    end
  end

  state.rebase = rebase
end

M.register = function(meta)
  meta.update_rebase_status = M.update_rebase_status
end

return M
