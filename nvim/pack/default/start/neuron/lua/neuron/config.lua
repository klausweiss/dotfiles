local Path = require("plenary/path")

local Config = {}
Config.__index = Config

function Config:validate()
  vim.validate {
    neuron_dir = {self.neuron_dir, 'string'},
    mappings = {self.mappings, 'boolean'},
    virtual_titles = {self.virtual_titles, 'boolean'},
    run = {self.run, 'function'},
    leader = {self.leader, 'string'},
    gen_cache_on_write = {self.gen_cache_on_write, 'boolean'},
    virt_text_highlight = {self.virt_text_highlight, 'string'},
  }

  if not Path:new(self.path):exists() then
    error(string.format("The path supplied for the neuron_dir does not exist"))
  end
end

function Config:extend(user_config)
  for k, v in pairs(user_config) do self[k] = v end
  self:validate()
end

function Config:after_extend()
  self.neuron_dir = vim.fn.expand(self.neuron_dir)
end

---Please use the neuron setup function at the base of the repo
function Config:setup(user_config)
  self:extend(user_config)
  self:after_extend()
end

return setmetatable({
  neuron_dir = vim.fn.expand("~/neuron"),
  mappings = true,
  virtual_titles = true,
  run = function() end,
  leader = "gz", -- the leader key to for all mappings
  gen_cache_on_write = true,
  virt_text_highlight = "Comment",
}, Config)
