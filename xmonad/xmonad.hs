import UnliftIO.Directory (getHomeDirectory)
import XMonad
import XMonad.Config.Desktop
import XMonad.Util.EZConfig (additionalKeysP)
import XMonad.Util.SpawnOnce

main =
    xmonad $
        desktopConfig
            { terminal = "kitty"
            , modMask = mod4Mask
            , startupHook = myStartupHook
            }
            `additionalKeysP` [ ("M-p", spawn "rofi -show combi")
                              ]

myStartupHook :: X ()
myStartupHook = do
    spawnOnce "polybar"
    spawnOnce "nm-applet"
    spawnOnce "redshift -l 42.65:18.1"
    home <- getHomeDirectory
    let wallpaperPath = home <> "/.dotfiles/xorg/wallpaper.png"
    spawnOnce $ "feh --bg-fill " <> wallpaperPath
