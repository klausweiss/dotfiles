local popup = require("neogit.lib.popup")
local status = require 'neogit.status'
local GitCommandHistory = require("neogit.buffers.git_command_history")

local function create(pos)
  popup.create(
    "NeogitHelpPopup",
    {},
    {},
    {
      {
        {
          key = "p",
          description = "Pull",
          callback = function()
            require('neogit.popups.pull').create()
          end
        },
      },
      {
        {
          key = "P",
          description = "Push",
          callback = function()
            require('neogit.popups.push').create()
          end
        },
      },
      {
        {
          key = "Z",
          description = "Stash",
          callback = function(popup)
            require('neogit.popups.stash').create(popup.env.pos)
          end
        },
      },
      {
        {
          key = "D",
          description = "Diff",
          callback = function ()
            require('neogit.popups.diff').create()
          end
        }
      },
      {
        {
          key = "L",
          description = "Log",
          callback = function()
            require('neogit.popups.log').create()
          end
        },
      },
      {
        {
          key = "c",
          description = "Commit",
          callback = function()
            require('neogit.popups.commit').create()
          end
        },
      },
      {
        {
          key = "b",
          description = "Branch",
          callback = function ()
            require('neogit.popups.branch').create()
          end
        }
      },
      {
        {
          key = "$",
          description = "Git Command History",
          callback = function()
            GitCommandHistory:new():show()
          end
        },
      },
      {
        {
          key = "<c-r>",
          description = "Refresh Status Buffer",
          callback = function()
            status.refresh(true)
          end
        },
      },
    }, { pos = pos })
end

return {
  create = create
}
