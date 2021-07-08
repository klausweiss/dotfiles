local neuron = require("neuron")
local M = {}

function M.gen_from_zettels(entry)
  local value = string.format("%s/%s", neuron.config.neuron_dir, entry.zettelPath)
  local display = entry.zettelTitle
  return {
    display = display,
    value = value,
    ordinal = display,
    id = entry.zettelID,
  }
end

--- Backlinks or uplinks
function M.gen_from_links(entry)
  local not_folgezettal = entry[2]
  return M.gen_from_zettels(not_folgezettal)
end

function M.gen_from_tags(entry)
  local display = entry.name
  return {
    display = display,
    value = display,
    ordinal = display,
  }
end

return M
