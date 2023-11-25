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
            `additionalKeysP` myKeys

launcherCmd = "rofi -show combi"
browserCmd = "firefox"
volumeUpCmd = "pactl set-sink-volume @DEFAULT_SINK@ +10%"
volumeDownCmd = "pactl set-sink-volume @DEFAULT_SINK@ -10%"
volumeMuteCmd = "pactl set-sink-mute @DEFAULT_SINK@ toggle"
microphoneMuteCmd = "pactl set-source-mute @DEFAULT_SOURCE@ toggle"
brightnessUpCmd = "lux -a 5%"
brightnessDownCmd = "lux -s 5%"
screenshotCmd = "flameshot gui"

myKeys =
    [ ("M-p", spawn launcherCmd)
    , ("M-w", spawn browserCmd)
    , ("<XF86MonBrightnessUp>", spawn brightnessUpCmd)
    , ("<XF86MonBrightnessDown>", spawn brightnessDownCmd)
    , ("<XF86AudioMute>", spawn volumeMuteCmd)
    , ("<XF86AudioLowerVolume>", spawn volumeDownCmd)
    , ("<XF86AudioRaiseVolume>", spawn volumeUpCmd)
    , ("<XF86AudioMicMute>", spawn microphoneMuteCmd)
    , ("<Print>", spawn screenshotCmd)
    ]

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
