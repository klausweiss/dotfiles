local beautiful = require("beautiful")
local gears = require("gears")


local function set_wallpaper(screen)
    -- Wallpaper
    if beautiful.wallpaper then
        local wallpaper = beautiful.wallpaper
        -- If wallpaper is a function, call it with the screen
        if type(wallpaper) == "function" then
            wallpaper = wallpaper(screen)
        end
        gears.wallpaper.maximized(wallpaper, screen, true)
    end
end


return {
   set_wallpaper = set_wallpaper,
}
