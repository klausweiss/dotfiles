local battery_api = require("lib/api/battery/upower")

local api_table = {
   _instance = nil,
}

local function mk_api()
   return battery_api {
      timeout = 60,
   }
end

local api_metatable = {
   __index = function (t, key)
      if key == "instance" then
	  if t._instance == nil then
	     local api = mk_api()
	     t._instance = api
	  end
	  return t._instance
      end
   end,
}

return setmetatable(api_table, api_metatable)
