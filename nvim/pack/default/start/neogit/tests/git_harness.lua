local status = require("neogit.status")
local a = require("plenary.async")
local M = {}

local project_dir = vim.fn.getcwd()

-- very naiive implementation, we only use this to generate unique folder names
local function random_string(length)
  math.randomseed(os.clock() ^ 5)

  local res = ""
  for _ = 1, length do
    local r = math.random(97, 122)
    res = res .. string.char(r)
  end
  return res
end

local function prepare_repository(dir)
  vim.api.nvim_set_current_dir(project_dir)
  local cmd = string.format(
    [[
mkdir -p tmp
cp -r tests/.repo/ tmp/%s
cp -r tmp/%s/.git.orig/ tmp/%s/.git/
]],
    dir,
    dir,
    dir
  )
  vim.fn.system(cmd)
  vim.api.nvim_set_current_dir("tmp/" .. dir)
end

local function cleanup_repository(dir)
  vim.api.nvim_set_current_dir(project_dir)

  vim.fn.system(string.format([[ rm -rf tmp/%s ]], dir))
end

function M.in_prepared_repo(cb)
  return function()
    local dir = "neogit_test_" .. random_string(5)
    prepare_repository(dir)
    vim.cmd("Neogit")
    a.util.block_on(status.reset)
    local _, err = pcall(cb)
    cleanup_repository(dir)
    if err ~= nil then
      error(err)
    end
  end
end

function M.get_git_status(files)
  local result = vim.api.nvim_exec("!git status -s --porcelain=1 -- " .. (files or ""), true)
  local lines = vim.split(result, "\n")
  local output = {}
  for i = 3, #lines do
    table.insert(output, lines[i])
  end
  return table.concat(output, "\n")
end

function M.get_git_diff(files, flags)
  local result = vim.api.nvim_exec("!git diff " .. (flags or "") .. " -- " .. (files or ""), true)
  local lines = vim.split(result, "\n")
  local output = {}
  for i = 5, #lines do
    table.insert(output, lines[i])
  end
  return table.concat(output, "\n")
end

function M.get_git_branches()
  local result = vim.api.nvim_exec("!git branch --list --all", true)
  local lines = vim.split(result, "\n")
  local output = {}
  local current_branch = nil
  for _, l in ipairs(lines) do
    local branch_state, name = l:match("^([* ]) (.+)")
    if branch_state == "*" then
      current_branch = name
    end
    if name then
      table.insert(output, name)
    end
  end
  return output, current_branch
end

function M.get_current_branch()
  local result = vim.api.nvim_exec("!git branch --show-current", true)
  local lines = vim.split(result, "\n")
  return lines[#lines - 1]
end

return M
