import UnliftIO.Directory (getHomeDirectory)
import XMonad
import XMonad.Config.Desktop
import XMonad.Util.Cursor (setDefaultCursor)
import XMonad.Util.EZConfig (additionalKeysP)
import XMonad.Util.SpawnOnce

main =
    xmonad $
        desktopConfig
            { terminal = "kitty"
            , modMask = mod4Mask
            , startupHook = myStartupHook
            }
            `additionalKeysP` [ ("M-p", spawn launcherCmd)
                              , ("M-w", spawn browserCmd)
                              ]

launcherCmd = "rofi -show combi"
browserCmd = "firefox"

myStartupHook :: X ()
myStartupHook = do
    setDefaultCursor xC_left_ptr
    startBars
    startRedshift
    setWallpaper
    populateTray
    capslockIsCtrl

capslockIsCtrl = do
    spawnOnce "setxkbmap pl"
    spawnOnce "setxkbmap -option shift:both_capslock"
    spawnOnce "setxkbmap -option caps:ctrl_modifier"

setWallpaper = do
    home <- getHomeDirectory
    let wallpaperPath = home <> "/.dotfiles/xorg/wallpaper.png"
    spawnOnce $ "feh --bg-fill " <> wallpaperPath

startBars = do
    spawnOnce "polybar"

startRedshift = do
    spawnOnce "redshift -l 42.65:18.1"

populateTray = do
    spawnOnce "nm-applet"
    spawnOnce "blueman-applet"
    spawnOnce "flameshot"
