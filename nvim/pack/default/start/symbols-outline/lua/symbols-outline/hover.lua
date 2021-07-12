local vim = vim

local main = require('symbols-outline')
local util = vim.lsp.util

local M = {}

local function get_hover_params(node, winnr)
    local bufnr = vim.api.nvim_win_get_buf(winnr)
    local fn = "file://" .. vim.api.nvim_buf_get_name(bufnr)

    return {
        textDocument = {uri = fn},
        position = {line = node.line, character = node.character},
        bufnr = bufnr
    }
end

-- handler yoinked from the default implementation
function M.show_hover()
    local current_line = vim.api.nvim_win_get_cursor(main.state.outline_win)[1]
    local node = main.state.flattened_outline_items[current_line]

    local hover_params = get_hover_params(node, main.state.code_win)

    vim.lsp.buf_request(hover_params.bufnr, "textDocument/hover", hover_params,
                        function(_, method, result)

        util.focusable_float(method, function()
            if not (result and result.contents) then
                -- return { 'No information available' }
                return
            end

            local markdown_lines = util.convert_input_to_markdown_lines(
                                       result.contents)
            markdown_lines = util.trim_empty_lines(markdown_lines)
            if vim.tbl_isempty(markdown_lines) then
                -- return { 'No information available' }
                return
            end
            local bufnr, winnr = util.fancy_floating_markdown(markdown_lines, {
                border = {
                    {"┌", "FloatBorder"}, {"─", "FloatBorder"},
                    {"┐", "FloatBorder"}, {"│", "FloatBorder"},
                    {"┘", "FloatBorder"}, {"─", "FloatBorder"},
                    {"└", "FloatBorder"}, {"│", "FloatBorder"}
                }
            })
            util.close_preview_autocmd({
                "CursorMoved", "BufHidden", "InsertCharPre", "BufWipeout", "BufDelete", "WinClosed"
            }, winnr)

            return bufnr, winnr
        end)
    end)
    -- kind of a hack but we want the main.state to always be the latest, so unload
    -- this module for the next time it is called its gonna be F R E S H
    -- package.loaded["symbols-outline.hover"] = nil
end

return M
