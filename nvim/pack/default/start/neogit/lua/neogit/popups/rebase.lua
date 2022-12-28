local cli = require("neogit.lib.git.cli")
local git = require("neogit.lib.git")
local popup = require("neogit.lib.popup")
local CommitSelectViewBuffer = require("neogit.buffers.commit_select_view")
local rebase = require("neogit.lib.git.rebase")

local M = {}
local a = require("plenary.async")

function M.create()
  local status = require("neogit.status")
  local p = popup
    .builder()
    :name("NeogitRebasePopup")
    :action_if(status and status.repo.rebase.head, "r", "Continue rebase", function()
      rebase.continue()
      a.util.scheduler()
      status.refresh(true, "rebase_continue")
    end)
    :action_if(status and status.repo.rebase.head, "s", "Skip rebase", function()
      rebase.skip()
      a.util.scheduler()
      status.refresh(true, "rebase_skip")
    end)
    :action_if(status and status.repo.rebase.head, "a", "Abort rebase", function()
      cli.rebase.abort.call_sync():trim()
      a.util.scheduler()
      status.refresh(true, "rebase_abort")
    end)
    :action(
      "p",
      "Rebase onto master",
      a.void(function()
        rebase.rebase_onto("master")
        a.util.scheduler()
        status.refresh(true, "rebase_master")
      end)
    )
    :action(
      "e",
      "Rebase onto elsewhere",
      a.void(function()
        local branch = git.branch.prompt_for_branch(git.branch.get_all_branches())
        rebase.rebase_onto(branch)
        a.util.scheduler()
        status.refresh(true, "rebase_elsewhere")
      end)
    )
    :action(
      "i",
      "Interactive",
      a.void(function()
        local commits = require("neogit.lib.git.log").list()

        local commit = CommitSelectViewBuffer.new(commits):open_async()

        if not commit then
          return
        end

        rebase.rebase_interactive(commit.oid)
        a.util.scheduler()
        status.refresh(true, "rebase_interactive")
      end)
    )
    :build()

  p:show()

  return p
end

return M
