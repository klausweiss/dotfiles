--
-- layout.lua
--

local floor = math.floor
local max = math.max
local min = math.min
local rshift = bit.rshift
local table_insert = table.insert

local buf_get_option = vim.api.nvim_buf_get_option --- @type function
local get_option = vim.api.nvim_get_option --- @type function
local strwidth = vim.api.nvim_strwidth --- @type function
local tabpagenr = vim.fn.tabpagenr --- @type function

local Buffer = require'bufferline.buffer'
local icons = require'bufferline.icons'
local options = require'bufferline.options'
local state = require'bufferline.state'

--- The number of sides of each buffer in the tabline.
local SIDES_OF_BUFFER = 2

--- @class bufferline.layout.data
--- @field actual_width integer
--- @field available_width integer
--- @field base_widths integer[]
--- @field buffers_width integer
--- @field padding_width integer
--- @field tabpages_width integer
--- @field used_width integer

--- @class bufferline.Layout
--- @field buffers integer[] different from `state.buffers` in that the `hide` option is respected. Only updated when calling `calculate_buffers_width`.
local Layout = {buffers = {}}

--- The number of characters needed to represent the tabpages.
--- @return integer width
function Layout.calculate_tabpages_width()
  local current = tabpagenr()
  local total   = tabpagenr('$')
  if not options.tabpages() or total == 1 then
    return 0
  end
  return 1 + tostring(current):len() + 1 + tostring(total):len() + 1
end

--- @param bufnr integer the buffer to calculate the width of
--- @param index integer the buffer's numerical index
--- @param render {buffer_index: boolean, buffer_number: boolean, diagnostics: bufferline.options.diagnostics, file_icon: boolean}
--- @return integer width
function Layout.calculate_buffer_width(bufnr, index, render)
  local buffer_data = state.get_buffer_data(bufnr)
  local buffer_name = buffer_data.name or '[no name]'
  local width

  if buffer_data.closing then
    width = buffer_data.real_width
  else
    local activity = Buffer.get_activity(bufnr) > 1 and 'active' or 'inactive'
    width = strwidth(options['icon_separator_' .. activity]()) + strwidth(buffer_name) -- separator + name

    if render.buffer_index then
      width = width + #tostring(index) + 1 -- buffer-index + space after buffer-index
    elseif render.buffer_number then
      width = width + #tostring(bufnr) + 1 -- buffer-number + space after buffer-index
    end

    if render.file_icon then
      --- @diagnostic disable-next-line:param-type-mismatch
      local file_icon = icons.get_icon(bufnr, '')
      width = width + strwidth(file_icon) + 1 -- icon + space after icon
    end

    Buffer.for_each_counted_enabled_diagnostic(bufnr, render.diagnostics, function(c, d, _)
      width = width + 1 + strwidth(d.icon) + #tostring(c) -- space before icon + icon + diagnostic count
    end)

    local is_pinned = state.is_pinned(bufnr)
    if options.closable() or is_pinned then
      width = width + strwidth(is_pinned and options.icon_pinned() or (
        buf_get_option(bufnr, 'modified') and options.icon_close_tab_modified() or options.icon_close_tab()
      )) -- close-or-pin-or-save-icon
    end

    width = width + 1 -- space after close-or-pin-or-save-icon
  end

  return width or 0
end

--- Calculate the width of the buffers
--- @return integer sum, integer[] widths
function Layout.calculate_buffers_width()
  Layout.buffers = Buffer.hide(state.buffers)

  local icons_option = options.icons()
  local render = {
    buffer_index = options.index_buffers(icons_option),
    buffer_number = options.number_buffers(icons_option),
    diagnostics = options.diagnostics(),
    file_icon = options.file_icons(icons_option),
  }

  local sum = 0
  local widths = {}

  for i, bufnr in ipairs(Layout.buffers) do
    local width = Layout.calculate_buffer_width(bufnr, i, render)
    sum = sum + width
    table_insert(widths, width)
  end

  return sum, widths
end

--- Calculate the current layout of the bufferline.
--- @return bufferline.layout.data
function Layout.calculate()
  local available_width = get_option'columns'
  available_width = available_width - state.offset.width

  local used_width, base_widths = Layout.calculate_buffers_width()
  local tabpages_width = Layout.calculate_tabpages_width()

  local buffers_width = available_width - tabpages_width

  local remaining_width              = max(buffers_width - used_width, 0)
  local remaining_width_per_buffer   = floor(remaining_width / #base_widths)
  -- PERF: faster than `floor(remaining_width_per_buffer / SIDES_OF_BUFFER)`.
  --       if `SIDES_OF_BUFFER` changes, this will have to go back to `floor`.
  local remaining_padding_per_buffer = rshift(remaining_width_per_buffer, 1)
  local padding_width                = max(options.minimum_padding(), min(remaining_padding_per_buffer, options.maximum_padding()))
  local actual_width                 = used_width + (#base_widths * padding_width * SIDES_OF_BUFFER)

  return {
    actual_width = actual_width,
    available_width = available_width,
    base_widths = base_widths,
    buffers_width = buffers_width,
    padding_width = padding_width,
    tabpages_width = tabpages_width,
    used_width = used_width,
  }
end

--- @return {[integer]: integer} position_by_bufnr
function Layout.calculate_buffers_position_by_buffer_number()
  local current_position = 0
  local layout = Layout.calculate()
  local positions = {}

  for i, buffer_number in ipairs(Layout.buffers) do
    positions[buffer_number] = current_position
    local width = layout.base_widths[i] + (2 * layout.padding_width)
    current_position = current_position + width
  end

  return positions
end

--- @param base_width integer
--- @param padding_width integer
--- @return integer width
function Layout.calculate_width(base_width, padding_width)
  return base_width + (padding_width * SIDES_OF_BUFFER)
end

return Layout
