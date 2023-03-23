local pickers = require("telescope.pickers")
local finders = require("telescope.finders")
local sorters = require("telescope.sorters")
local actions = require("telescope.actions")
local action_state = require("telescope.actions.state")
local entry_display = require("telescope.pickers.entry_display")
local themes = require("telescope.themes")

---@alias TelescopeEntry {hint: string, label: string, command: fun(), id: integer}
---@alias CustomOptions {title: string, callback: fun(bufnr: integer)}

local M = {}

-- Accounts for the vertical padding implicit in the dropdown.
local MENU_PADDING = 4

local function execute_command(bufnr)
  local selection = action_state.get_selected_entry()
  actions.close(bufnr)
  local cmd = selection.command
  if cmd then
    local success, msg = pcall(cmd)
    if not success then vim.notify(msg, vim.log.levels.ERROR) end
  end
end

local function command_entry_maker(max_width)
  local make_display = function(en)
    local displayer = entry_display.create({
      separator = en.hint ~= "" and " • " or "",
      items = {
        { width = max_width },
        { remaining = true },
      },
    })

    return displayer({
      { en.label, "Type" },
      { en.hint, "Comment" },
    })
  end
  return function(entry)
    return {
      ordinal = entry.id,
      command = entry.command,
      hint = entry.hint,
      label = entry.label,
      display = make_display,
    }
  end
end

local function get_max_length(commands)
  local max = 0
  for _, value in ipairs(commands) do
    max = #value.label > max and #value.label or max
  end
  return max
end

---@param items TelescopeEntry[]
---@param opts CustomOptions
---@return table
local function picker_opts(items, opts)
  local callback = opts.callback or execute_command
  return {
    prompt_title = opts.title,
    finder = finders.new_table({
      results = items,
      entry_maker = command_entry_maker(get_max_length(items)),
    }),
    sorter = sorters.get_generic_fuzzy_sorter(),
    attach_mappings = function(_, map)
      map("i", "<CR>", callback)
      map("n", "<CR>", callback)
      -- If the return value of `attach_mappings` is true, then the other
      -- default mappings are still applies.
      -- Return false if you don't want any other mappings applied.
      -- A return value _must_ be returned. It is an error to not return anything.
      return true
    end,
  }
end

---The options use to create the custom telescope picker menu's for flutter-tools
---@param items TelescopeEntry[]
---@param user_opts table?
---@param opts CustomOptions?
function M.get_config(items, user_opts, opts)
  opts = vim.tbl_deep_extend("keep", user_opts or {}, opts or {})
  return themes.get_dropdown(vim.tbl_deep_extend("keep", picker_opts(items, opts), {
    previewer = false,
    layout_config = { height = #items + MENU_PADDING },
  }))
end

function M.commands(opts)
  local commands = {}

  local commands_module = require("flutter-tools.commands")
  if commands_module.is_running() then
    commands = {
      {
        id = "flutter-tools-hot-reload",
        label = "Flutter tools: Hot reload",
        hint = "Reload a running flutter project",
        command = require("flutter-tools.commands").reload,
      },
      {
        id = "flutter-tools-hot-restart",
        label = "Flutter tools: Hot restart",
        hint = "Restart a running flutter project",
        command = require("flutter-tools.commands").restart,
      },
      {
        id = "flutter-tools-visual-debug",
        label = "Flutter tools: Visual Debug",
        hint = "Add the visual debugging overlay",
        command = require("flutter-tools.commands").visual_debug,
      },
      {
        id = "flutter-tools-quit",
        label = "Flutter tools: Quit",
        hint = "Quit running flutter project",
        command = require("flutter-tools.commands").quit,
      },
      {
        id = "flutter-tools-detach",
        label = "Flutter tools: Detach",
        hint = "Quit running flutter project but leave the process running",
        command = require("flutter-tools.commands").detach,
      },
      {
        id = "flutter-tools-widget-inspector",
        label = "Flutter tools: Widget Inspector",
        hint = "Toggle the widget inspector",
        command = require("flutter-tools.commands").widget_inspector,
      },
      {
        id = "flutter-tools-construction-lines",
        label = "Flutter tools: Construction Lines",
        hint = "Display construction lines",
        command = require("flutter-tools.commands").construction_lines,
      },
    }
  else
    commands = {
      {
        id = "flutter-tools-run",
        label = "Flutter tools: Run",
        hint = "Start a flutter project",
        command = require("flutter-tools.commands").run,
      },
    }
  end

  vim.list_extend(commands, {
    {
      id = "flutter-tools-pub-get",
      label = "Flutter tools: Pub get",
      hint = "Run pub get in the project directory",
      command = require("flutter-tools.commands").pub_get,
    },
    {
      id = "flutter-tools-pub-upgrade",
      label = "Flutter tools: Pub upgrade",
      hint = "Run pub upgrade in the project directory",
      command = require("flutter-tools.commands").pub_upgrade,
    },
    {
      id = "flutter-tools-list-devices",
      label = "Flutter tools: List Devices",
      hint = "Show the available physical devices",
      command = require("flutter-tools.devices").list_devices,
    },
    {
      id = "flutter-tools-list-emulators",
      label = "Flutter tools: List Emulators",
      hint = "Show the available emulator devices",
      command = require("flutter-tools.devices").list_emulators,
    },
    {
      id = "flutter-tools-open-outline",
      label = "Flutter tools: Open Outline",
      hint = "Show the current files widget tree",
      command = require("flutter-tools.outline").open,
    },
    {
      id = "flutter-tools-generate",
      label = "Flutter tools: Generate ",
      hint = "Generate code",
      command = require("flutter-tools.commands").generate,
    },
    {
      id = "flutter-tools-clear-dev-log",
      label = "Flutter tools: Clear Dev Log",
      hint = "Clear previous logs in the output buffer",
      command = require("flutter-tools.log").clear,
    },
  })

  local dev_tools = require("flutter-tools.dev_tools")

  if dev_tools.is_running() then
    vim.list_extend(commands, {
      {
        id = "flutter-tools-copy-profiler-url",
        label = "Flutter tools: Copy Profiler Url",
        hint = "Run the app and the DevTools first",
        command = require("flutter-tools.commands").copy_profiler_url,
      },
      {
        id = "flutter-tools-open-dev-tools",
        label = "Flutter tools: Open Dev Tools",
        hint = "Run the app and the Dev Tools first",
        command = require("flutter-tools.commands").open_dev_tools,
      },
    })
  else
    vim.list_extend(commands, {
      {
        id = "flutter-tools-start-dev-tools",
        label = "Flutter tools: Start Dev Tools",
        hint = "Open flutter dev tools in the browser",
        command = require("flutter-tools.dev_tools").start,
      },
    })
  end

  pickers.new(M.get_config(commands, opts, { title = "Flutter tools commands" })):find()
end

local function execute_fvm_use(bufnr)
  local selection = action_state.get_selected_entry()
  actions.close(bufnr)
  local cmd = selection.command
  if cmd then
    local success, msg = pcall(cmd, selection.ordinal)
    if not success then vim.notify(msg, vim.log.levels.ERROR) end
  end
end

function M.fvm(opts)
  local commands = require("flutter-tools.commands")
  commands.fvm_list(function(sdks)
    opts = opts and not vim.tbl_isempty(opts) and opts
      or themes.get_dropdown({
        previewer = false,
        layout_config = {
          height = #sdks + MENU_PADDING,
        },
      })

    local sdk_entries = {}
    for _, sdk in pairs(sdks) do
      table.insert(sdk_entries, {
        id = sdk.name,
        label = sdk.name,
        hint = sdk.status and "(" .. sdk.status .. ")" or "",
        command = commands.fvm_use,
      })
    end

    pickers
      .new(M.get_config(sdk_entries, opts, {
        title = "Change Flutter SDK",
        callback = execute_fvm_use,
      }))
      :find()
  end)
end

return M
