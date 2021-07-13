local a = require 'plenary.async_lib'
local async, await, scheduler = a.async, a.await, a.scheduler
local cli = require('neogit.lib.git.cli')
local util = require('neogit.lib.util')
local input = require('neogit.lib.input')
local M = {}

local function contains(table, val)
   for i=1,#table do
      if table[i] == val then
         return true
      end
   end
   return false
end

local function parse_branches(branches)
  local other_branches = {}
  for _, b in ipairs(branches) do
    local branch_name = b:match('^  (.+)')
    if branch_name then 
      table.insert(other_branches, branch_name) 
    end
  end

  return other_branches
end

local get_local_branches = async(function ()
  local branches = await(cli.branch
    .list
    .call())

  return parse_branches(branches)
end)

local get_all_branches = async(function ()
  local branches = await(cli.branch
    .list
    .all
    .call())

  return parse_branches(branches)
end)

local function prompt_for_branch(options)
  local chosen = input.get_user_input_with_completion('branch > ', options)
  if not chosen or chosen == '' then return nil end
  if not contains(options, chosen) then
    print("ERROR: Branch doesn't exit")
    return
  end
  return chosen
end

M.checkout_local = async(function ()
  local branches = await(get_local_branches())

  await(scheduler())
  local chosen = prompt_for_branch(branches)
  if not chosen then return end
  await(cli.checkout.branch(chosen).call())
end)

M.checkout = async(function ()
  local branches = await(get_all_branches())

  await(scheduler())
  local chosen = prompt_for_branch(branches)
  if not chosen then return end
  await(cli.checkout.branch(chosen).call())
end)

M.create = async(function ()
  await(scheduler())
  local name = input.get_user_input('branch > ')
  if not name or name == '' then return end

  await(cli.branch.name(name).call())

  return name
end)

M.delete = async(function ()
  local branches = await(get_all_branches())

  await(scheduler())
  local chosen = prompt_for_branch(branches)
  if not chosen then return end
  await(cli.branch.delete.name(chosen).call())

  return chosen
end)

M.checkout_new = async(function ()
  await(scheduler())
  local name = input.get_user_input('branch > ')
  if not name or name == '' then return end
  await(cli.checkout
    .new_branch(name)
    .call())
end)

M.prompt_for_branch = prompt_for_branch

return M
