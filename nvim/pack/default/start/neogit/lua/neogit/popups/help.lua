local popup = require("neogit.lib.popup")
local status = require 'neogit.status'
local GitCommandHistory = require("neogit.buffers.git_command_history")

local M = {}

function M.create(env)
  local p = popup.builder()
    :name("NeogitHelpPopup")
    :action("p", "Pull", function()
      require('neogit.popups.pull').create()
    end)
    :action("P", "Push", function()
      require('neogit.popups.push').create()
    end)
    :action("Z", "Stash", function(popup)
      require('neogit.popups.stash').create(popup.env.get_stash())
    end)
    :action("L", "Log", function()
      require('neogit.popups.log').create()
    end)
    :new_action_group()
    :action("c", "Commit", function()
      require('neogit.popups.commit').create()
    end)
    :action("b", "Branch", function()
      require('neogit.popups.branch').create()
    end)
    :action("$", "Git Command History", function()
      GitCommandHistory:new():show()
    end)
    :action("<c-r>", "Refresh Status Buffer", function()
      status.refresh(true)
    end)
    :env(env)
    :build()

  p:show()
  
  return p 
end

return M
