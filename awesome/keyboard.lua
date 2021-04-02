local awful = require("awful")
local gears = require("gears")
local menubar = require("menubar")

local clients = require("lib/clients")
local layouts = require("lib/layouts")
local programs = require("programs")
local rules = require("rules")
local run = require("lib/procedures").run
local screens = require("lib/screens")
local tags = require("tags")
local sound_api = require("lib/api/sound").instance

function mk_key (args)
   local modifiers = table.pack(table.unpack(args, 1, #args - 1))
   local key_symbol = args[#args]
   return awful.key(
      modifiers, key_symbol, args.fun or args.clientfun,
      {
	 description = args.desc,
	 group = args.group,
      }
   )
end

local globalkeys = {}
local clientkeys = {}

local setkey = setmetatable({}, {
      __call = function(self, args)
	 local key = mk_key(args)
	 if args.fun then
	    table.insert(globalkeys, key)
	 elseif args.clientfun then
	    table.insert(clientkeys, key)
	 end
      end,
})

local ALT = "Mod1"
local CTRL = "Control"
local ENTER = "Return"
local SHIFT = "Shift"
local SUPER = "Mod4"
local TAB = "Tab"
local LEFT = "Left"
local RIGHT = "Right"
local UP = "Up"
local DOWN = "Down"

-- layouts
setkey { ALT, " ",
	 fun = layouts.next_layout }
setkey { ALT, SHIFT, " ",
	 fun = layouts.prev_layout }
-- clients
setkey { ALT, TAB,
	 fun = clients.focus_next }
setkey { ALT, SHIFT, TAB,
	 fun = clients.focus_prev }
-- screens
setkey { ALT, "t",
	 fun = screens.focus_left_screen }
setkey { ALT, LEFT,
	 fun = screens.focus_left_screen }
setkey { ALT, "s",
	 fun = screens.focus_right_screen }
setkey { ALT, RIGHT,
	 fun = screens.focus_right_screen }
setkey { ALT, "p",
	 fun = screens.focus_up_screen }
setkey { ALT, UP,
	 fun = screens.focus_up_screen }
setkey { ALT, "n",
	 fun = screens.focus_down_screen }
setkey { ALT, DOWN,
	 fun = screens.focus_down_screen }
setkey { ALT, SHIFT, "t",
	 fun = screens.move_client_to_left_screen  }
setkey { ALT, SHIFT, LEFT,
	 fun = screens.move_client_to_left_screen  }
setkey { ALT, SHIFT, "s",
	 fun = screens.move_client_to_right_screen  }
setkey { ALT, SHIFT, RIGHT,
	 fun = screens.move_client_to_right_screen  }
setkey { ALT, SHIFT, "p",
	 fun = screens.move_client_to_up_screen  }
setkey { ALT, SHIFT, UP,
	 fun = screens.move_client_to_up_screen  }
setkey { ALT, SHIFT, "n",
	 fun = screens.move_client_to_down_screen  }
setkey { ALT, SHIFT, DOWN,
	 fun = screens.move_client_to_down_screen  }
-- awesome
setkey { ALT, SHIFT, "r",
	 fun = awesome.restart  }
setkey { ALT, SHIFT, "q",
	 fun = awesome.quit }
-- keyboard action keys
setkey { "Print",
	 fun = run(programs.screenshooter) }
setkey { "XF86MonBrightnessUp",
	 fun = run(programs.brightness_up) }
setkey { "XF86MonBrightnessDown",
	 fun = run(programs.brightness_down) }
setkey { "XF86AudioRaiseVolume",
	 fun = function () sound_api.output_volume_up(5) end }
setkey { "XF86AudioLowerVolume",
	 fun = function () sound_api.output_volume_down(5) end }
setkey { "XF86AudioMute",
	 fun = sound_api.toggle_mute_output }
setkey { SHIFT, "XF86AudioMute",
	 fun = sound_api.toggle_mute_input}
setkey { "XF86Launch5",
	 fun = sound_api.set_output_type_to_headphones }
setkey { "XF86Launch6",
	 fun = sound_api.set_output_type_to_speakers}
setkey { "XF86Tools",
	 fun = run(programs.refresh_displays) }
-- launcher
setkey { CTRL, SHIFT, " ",
	 fun = run(programs.launcher) }
setkey { SUPER, " ",
	 fun = menubar.show }
-- programs
setkey { ALT, CTRL, ENTER,
	 fun = run(programs.terminal) }
setkey { ALT, SHIFT, "e",
	 fun = run(programs.file_manager) }
setkey { ALT, SHIFT, "w",
	 fun = run(programs.internet_browser) }
setkey { ALT, SHIFT, "m",
	 fun = run(programs.mail_client) }
setkey { ALT, SHIFT, "i",
	 fun = run("signal-desktop") }
setkey { ALT, SHIFT, "l",
	 fun = run(programs.screen_lock) }
setkey { ALT, SHIFT, "x",
	 fun = run("emacs") }
setkey { ALT, SHIFT, "d",
	 fun = run("codium") }
setkey { ALT, SHIFT, "k",
	 fun = run("zim") }


for i, _tagname in ipairs(tags.tags) do
   local keynum = "#" .. (i + 9)
   setkey { CTRL, keynum,
	    fun = tags.show_tag_by_index(i) }
   setkey { ALT, CTRL, keynum,
	    fun = tags.toggle_tag_by_index(i) }
   setkey { CTRL, SHIFT, keynum,
	    fun = tags.move_focused_client_to_tag(i) }
   setkey { ALT, CTRL, SHIFT, keynum,
	    fun = tags.toggle_tag_on_focused_client(i) }
end

globalkeys = gears.table.join(table.unpack(globalkeys)) -- flatten
root.keys(globalkeys)



setkey { ALT, SHIFT, "z",
	 clientfun = clients.toggle_fullscreen_client }
setkey { ALT, SHIFT, "m",
	 clientfun = clients.toggle_maximize_client }
setkey { ALT, "w",
	 clientfun = clients.close_client }
setkey { ALT, SHIFT, ENTER,
	 clientfun = layouts.select_main_client }


clientkeys = gears.table.join(table.unpack(clientkeys)) -- flatten
rules.add_rule(
   { rule = { },
     properties = { 
	keys = clientkeys,
     },
   }
)

return {
   ALT = ALT,
   CTRL = CTRL,
   ENTER = ENTER,
   SHIFT = SHIFT,
   SUPER = SUPER,
   TAB = TAB,

   clients_modkey = ALT,
}
