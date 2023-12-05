import UnliftIO.Directory (getHomeDirectory)
import XMonad
import XMonad.Config.Desktop
import XMonad.Layout.Grid
import XMonad.Layout.Spacing (spacing)
import XMonad.Util.Cursor (setDefaultCursor)
import XMonad.Util.EZConfig (additionalKeysP)
import XMonad.Util.SpawnOnce
import XMonad.Util.Themes

main =
    xmonad $
        desktopConfig
            { terminal = terminalCmd
            , modMask = mod4Mask
            , startupHook = myStartupHook
            , workspaces = myWorkspaces
            , manageHook = myManageHook <> manageHook desktopConfig
            , layoutHook = myLayoutHook
            }
            `additionalKeysP` myKeys

terminalCmd = "kitty"
launcherCmd = "rofi -show combi"
browserCmd = "firefox"
volumeUpCmd = "pactl set-sink-volume @DEFAULT_SINK@ +5%"
volumeDownCmd = "pactl set-sink-volume @DEFAULT_SINK@ -5%"
volumeMuteCmd = "pactl set-sink-mute @DEFAULT_SINK@ toggle"
microphoneMuteCmd = "pactl set-source-mute @DEFAULT_SOURCE@ toggle"
brightnessUpCmd = "lux -a 5%"
brightnessDownCmd = "lux -s 5%"
screenshotCmd = "flameshot gui"
lastScreenshotCmd = "flameshot gui --last-region"
lockScreenCmd = "i3lock -n --color 000000"
fileManagerCmd = "thunar"

myKeys =
    [ ("M-p", spawn launcherCmd)
    , ("M-q", kill)
    , ("M-S-w", spawn browserCmd)
    , ("M-S-e", spawn fileManagerCmd)
    , ("M-S-l", spawn lockScreenCmd)
    , ("<XF86MonBrightnessUp>", spawn brightnessUpCmd)
    , ("<XF86MonBrightnessDown>", spawn brightnessDownCmd)
    , ("<XF86AudioMute>", spawn volumeMuteCmd)
    , ("<XF86AudioLowerVolume>", spawn volumeDownCmd)
    , ("<XF86AudioRaiseVolume>", spawn volumeUpCmd)
    , ("<XF86AudioMicMute>", spawn microphoneMuteCmd)
    , ("<Print>", spawn screenshotCmd)
    , ("S-<Print>", spawn lastScreenshotCmd)
    ]

web = "1:web"
dev = "2:dev"
mail = "4:mail"
myWorkspaces =
    [ web
    , dev
    , "3"
    , mail
    ]
        <> fmap show [4 .. 9]

myLayoutHook = desktopLayoutModifiers . spacing 10 $ Grid ||| Full

myManageHook =
    mconcat
        [ className =? "firefox" --> doShift web
        , className =? "emacs" --> doShift dev
        , className =? "Signal" --> doShift mail
        , className =? "thunderbird" --> doShift mail
        , className =? "zoom" --> doShift mail
        , className =? "slack" --> doShift mail
        ]

myStartupHook :: X ()
myStartupHook = do
    startNotificationDaemon
    setDefaultCursor xC_left_ptr
    startBars
    startRedshift
    startCompositionManager
    startTouchpadGesturesManager
    setWallpaper
    setAutoScreenLock
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

startNotificationDaemon = do
    spawnOnce "dunst"

startBars = do
    spawnOnce "polybar"

startRedshift = do
    spawnOnce "redshift -l 42.65:18.1"

startCompositionManager = do
    spawnOnce "picom"

startTouchpadGesturesManager = do
    spawnOnce "touchegg"

populateTray = do
    spawnOnce "nm-applet"
    spawnOnce "blueman-applet"
    spawnOnce "flameshot"

setAutoScreenLock = do
    spawnOnce $ "xss-lock -- " <> lockScreenCmd
