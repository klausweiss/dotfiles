local notify = require "nvim-lsp-installer.notify"
local tsserverExtras = require "nvim-lsp-installer.extras.tsserver"

notify("nvim-tree adapter has been deprecated. More information: https://git.io/J6San.", vim.log.levels.WARN)

-- @deprecated
local M = {}

-- @deprecated
function M.connect()
    local ok, events = pcall(require, "nvim-tree.events")
    if not ok then
        return notify(
            "Unable to import nvim-tree module when connecting nvim-lsp-installer adapter.",
            vim.log.levels.ERROR
        )
    end

    events.on_node_renamed(function(payload)
        -- TODO: not do this when renaming folders
        tsserverExtras.rename_file(payload.old_name, payload.new_name)
    end)
end

-- @deprecated
return M
