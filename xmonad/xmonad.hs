import Data.Function ((&))
import qualified Data.Map as M
import UnliftIO.Directory (getHomeDirectory)
import XMonad
import XMonad.Actions.CycleWS (nextScreen, prevScreen, shiftNextScreen, shiftPrevScreen)
import XMonad.Actions.DynamicWorkspaceGroups
import XMonad.Actions.PhysicalScreens
import XMonad.Actions.TiledWindowDragging
import XMonad.Actions.Warp (warpToWindow)
import XMonad.Config.Desktop
import XMonad.Hooks.ManageDocks (ToggleStruts (..))
import XMonad.Hooks.SetWMName (setWMName)
import XMonad.Layout.DraggingVisualizer
import XMonad.Layout.GridVariants
import XMonad.Layout.Spacing (smartSpacing, spacing)
import XMonad.StackSet (greedyView, shift)
import qualified XMonad.StackSet as W
import XMonad.Util.Cursor (setDefaultCursor)
import XMonad.Util.EZConfig (additionalKeys, additionalKeysP, additionalMouseBindings)
import XMonad.Util.SpawnOnce
import XMonad.Util.Themes

main =
    xmonad $
        desktopConfig
            { terminal = terminalCmd
            , modMask = myModMask
            , startupHook = myStartupHook
            , workspaces = myWorkspaces
            , manageHook = myManageHook <> manageHook desktopConfig
            , layoutHook = myLayoutHook
            }
            `additionalKeysP` myKeysP
            `additionalMouseBindings` myMouseBindings

myModMask = mod4Mask

terminalCmd = "kitty"
launcherCmd = "rofi -show combi"
browserCmd = "firefox"
volumeUpCmd = "pactl set-sink-volume @DEFAULT_SINK@ +5%"
volumeDownCmd = "pactl set-sink-volume @DEFAULT_SINK@ -5%"
volumeMuteCmd = "pactl set-sink-mute @DEFAULT_SINK@ toggle"
microphoneMuteCmd = "pactl set-source-mute @DEFAULT_SOURCE@ toggle"
audioPauseCmd = "playerctl play-pause"
brightnessUpCmd = "lux -a 5%"
brightnessDownCmd = "lux -s 5%"
screenshotCmd = "flameshot gui"
lastScreenshotCmd = "flameshot gui --last-region"
lockScreenCmd = "i3lock -n --color 000000"
fileManagerCmd = "thunar"
powerMenuCmd = "rofi -show power-menu -modi power-menu:~/.dotfiles/rofi/rofi-power-menu/rofi-power-menu"

screenComparator = horizontalScreenOrderer
viewScreen' = viewScreen screenComparator
sendToScreen' = sendToScreen screenComparator

myKeysP =
    [ ("M-p", spawn launcherCmd)
    , ("M-q", kill)
    , ("C-M1-<Delete>", spawn powerMenuCmd)
    , ("M-S-w", spawn browserCmd)
    , ("M-S-e", spawn fileManagerCmd)
    , ("M-S-l", spawn lockScreenCmd)
    , ("<XF86MonBrightnessUp>", spawn brightnessUpCmd)
    , ("<XF86MonBrightnessDown>", spawn brightnessDownCmd)
    , ("<XF86AudioMute>", spawn volumeMuteCmd)
    , ("<XF86AudioLowerVolume>", spawn volumeDownCmd)
    , ("<XF86AudioRaiseVolume>", spawn volumeUpCmd)
    , ("<XF86AudioMicMute>", spawn microphoneMuteCmd)
    , ("<XF86AudioPause>", spawn audioPauseCmd)
    , ("<Print>", spawn screenshotCmd)
    , ("S-<Print>", spawn lastScreenshotCmd)
    , ("M-f", sendMessage ToggleStruts)
    , -- workspace-related shortcuts
      --   switch
      ("M-v", windows $ greedyView web)
    , ("M-x", windows $ greedyView dev)
    , ("M-c", windows $ greedyView notes)
    , ("M-i", windows $ greedyView mail)
    , ("M-a", windows $ greedyView "5")
    , ("M-e", windows $ greedyView "6")
    , --   move
      ("M-S-v", windows $ shift web)
    , ("M-S-x", windows $ shift dev)
    , ("M-S-c", windows $ shift notes)
    , ("M-S-i", windows $ shift mail)
    , ("M-S-a", windows $ shift "5")
    , ("M-S-e", windows $ shift "6")
    , -- monitor shortcuts
      --   switch
      ("M-u", viewScreen' 0 >> centerMouseOnWindow)
    , ("M-o", viewScreen' 1 >> centerMouseOnWindow)
    , ("M-y", viewScreen' 2 >> centerMouseOnWindow)
    , --   move
      ("M-S-u", sendToScreen' 0)
    , ("M-S-o", sendToScreen' 1)
    , ("M-S-y", sendToScreen' 2)
    ]
  where
    centerMouseOnWindow = warpToWindow 0.5 0.5
myMouseBindings :: [((ButtonMask, Button), Window -> X ())]
myMouseBindings =
    [ ((myModMask, button1), dragWindow)
    -- if it was  the following instead, floating mode wouldn't be disabled, but I haven't needed it once
    -- , ((myModMask .|. shiftMask, button1), dragWindow)
    ]

web = "1:web"
dev = "2:dev"
notes = "3:notes"
mail = "4:mail"
myWorkspaces =
    [ web
    , dev
    , notes
    , mail
    ]
        <> fmap show [5 .. 9]

myLayoutHook =
    draggingVisualizer $
        Grid (16 / 9) ||| Full
            & desktopLayoutModifiers
            & smartSpacing 10

myManageHook =
    mconcat
        [ internetBrowser --> doShift web
        , className =? "emacs" --> doShift dev
        , jetbrainsIde --> doShift dev
        , className =? "Signal" --> doShift mail
        , className =? "thunderbird" --> doShift mail
        , className =? "zoom" --> doShift mail
        , className =? "Slack" --> doShift mail
        , className =? "Logseq" --> doShift notes
        ]
  where
    jetbrainsIde :: Query Bool
    jetbrainsIde =
        foldl1
            (<||>)
            [ className =? "jetbrains-pycharm-ce"
            , className =? "jetbrains-pycharm"
            ]
    internetBrowser =
        foldl1
            (<||>)
            [ className =? "firefox"
            , className =? "google-chrome"
            ]

myStartupHook :: X ()
myStartupHook = do
    setupWorkspaceGroups
    fixJvmGuisNotWorking
    startNotificationDaemon
    setDefaultCursor xC_left_ptr
    startBars
    startRedshift
    startCompositionManager
    startTouchpadGesturesManager
    capslockIsCtrl
    setWallpaper
    setAutoScreenLock
    populateTray

setupWorkspaceGroups = do
    addRawWSGroup "test" []

fixJvmGuisNotWorking = do
    setWMName "LG3D"

capslockIsCtrl = do
    spawnOnce "setxkbmap pl -option shift:both_capslock -option caps:ctrl_modifier"

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
