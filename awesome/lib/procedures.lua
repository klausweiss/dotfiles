local awful = require("awful")

return {
   run = function (program)
      return function ()
	 awful.spawn(program)
      end
   end
}
