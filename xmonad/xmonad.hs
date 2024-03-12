{-# LANGUAGE QuasiQuotes #-}

import Data.Function ((&))
import qualified Data.Map as M
import Debug.Trace
import UnliftIO.Directory (getHomeDirectory)
import XMonad
import XMonad.Actions.CycleWS (nextScreen, prevScreen, shiftNextScreen, shiftPrevScreen)
import XMonad.Actions.DynamicWorkspaceGroups
import XMonad.Actions.PhysicalScreens
import XMonad.Actions.TiledWindowDragging
import XMonad.Actions.TopicSpace
import XMonad.Actions.Warp (warpToWindow)
import XMonad.Config.Desktop
import XMonad.Hooks.ManageDocks (ToggleStruts (..))
import XMonad.Hooks.Rescreen
import XMonad.Hooks.SetWMName (setWMName)
import XMonad.Layout.DraggingVisualizer
import XMonad.Layout.Fullscreen (fullscreenSupportBorder)
import XMonad.Layout.GridVariants
import XMonad.Layout.Spacing (smartSpacing, spacing)
import XMonad.StackSet (greedyView, shift)
import qualified XMonad.StackSet as W
import XMonad.Util.Cursor (setDefaultCursor)
import XMonad.Util.EZConfig (additionalKeys, additionalKeysP, additionalMouseBindings)
import XMonad.Util.NamedScratchpad
import XMonad.Util.SpawnOnce
import XMonad.Util.Themes hiding (TI)
import XmonadConfig.Presets (preset)

main =
    xmonad
        . fullscreenSupportBorder
        . addAfterRescreenHook myAfterRescreenHook
        $ desktopConfig
            { terminal = terminalCmd
            , modMask = myModMask
            , startupHook = startupHook desktopConfig >> myStartupHook
            , workspaces = topicNames topicItems
            , manageHook = myManageHook <> manageHook desktopConfig
            , layoutHook = myLayoutHook
            }
            `additionalKeysP` myKeysP
            `additionalMouseBindings` myMouseBindings

myModMask = mod4Mask

terminalCmd = "kitty"
launcherCmd = "rofi -show combi"
browserCmd = "firefox"
notesCmd = "logseq"
discordCmd = "discord"
signalCmd = "signal-desktop"
volumeUpCmd = "pactl set-sink-volume @DEFAULT_SINK@ +5%"
volumeDownCmd = "pactl set-sink-volume @DEFAULT_SINK@ -5%"
volumeMuteCmd = "pactl set-sink-mute @DEFAULT_SINK@ toggle"
microphoneMuteCmd = "pactl set-source-mute @DEFAULT_SOURCE@ toggle"
audioPauseCmd = "playerctl play-pause"
brightnessUpCmd = "lux -a 5%"
brightnessDownCmd = "lux -s 5%"
screenshotCmd = "flameshot gui"
lastScreenshotCmd = "flameshot gui --last-region"
lockScreenCmd = "xsecurelock"
idleHookCmd = "xidlehook --not-when-fullscreen --not-when-audio --timer 900 " <> lockScreenCmd <> "''"
fileManagerCmd = "thunar"
powerMenuCmd = "rofi -show power-menu -modi power-menu:~/.dotfiles/rofi/rofi-power-menu/rofi-power-menu"

screenComparator = horizontalScreenOrderer
viewScreen' = viewScreen screenComparator
getScreen' = getScreen screenComparator
sendToScreen' = sendToScreen screenComparator
onNextNeighbour' = onNextNeighbour screenComparator
onPrevNeighbour' = onPrevNeighbour screenComparator
switchTopic' = switchTopic myTopicConfig

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
      -- workspace groups
      ("M-<F7>", viewWSGroup "wg7")
    , ("M-<F8>", viewWSGroup "wg8")
    , ("M-<F9>", viewWSGroup "wg9")
    , ("M-S-<F7>", addCurrentWSGroup "wg7")
    , ("M-S-<F8>", addCurrentWSGroup "wg8")
    , ("M-S-<F9>", addCurrentWSGroup "wg9")
    , ("M-<F2>", viewWSGroup dev)
    , ("M-<F5>", viewWSGroup call)
    , -- scratchpads
      --   switch
      ("M-v", switchTopic' web)
    , ("M-x", switchTopic' dev)
    , ("M-c", switchTopic' notes)
    , ("M-i", switchTopic' chat)
    , ("M-a", switchTopic' call)
    , ("M-e", switchTopic' terminal')
    , --   move
      ("M-S-v", windows $ shift web)
    , ("M-S-x", windows $ shift dev)
    , ("M-S-c", windows $ shift notes)
    , ("M-S-i", windows $ shift chat)
    , ("M-S-a", windows $ shift call)
    , ("M-S-e", windows $ shift terminal')
    , -- monitor shortcuts
      --   switch
      ("M-u", viewScreen' 0 >> centerMouseOnWindow)
    , ("M-o", viewScreen' 1 >> centerMouseOnWindow)
    , ("M-y", viewScreen' 2 >> centerMouseOnWindow)
    , ("M-l", onPrevNeighbour' W.view >> centerMouseOnWindow)
    , ("M-w", onNextNeighbour' W.view >> centerMouseOnWindow)
    , --   move
      ("M-S-u", sendToScreen' 0)
    , ("M-S-o", sendToScreen' 1)
    , ("M-S-y", sendToScreen' 2)
    ]
  where
    centerMouseOnWindow = warpToWindow 0.5 0.5

myMouseBindings :: [((ButtonMask, Button), Window -> X ())]
myMouseBindings =
    [ ((myModMask .|. shiftMask, button1), dragWindow)
    ]

web = "1:web"
dev = "2:dev"
notes = "3:notes"
chat = "4:chat"
call = "5:call"
terminal' = "6:term"

topicItems =
    let firstTopics =
            [ inHome web (spawn browserCmd)
            , inHome dev (return ())
            , inHome notes (spawn notesCmd)
            , inHome
                chat
                ( do
                    spawnOnce discordCmd
                    spawnOnce signalCmd
                    return ()
                )
            , inHome call (return ())
            , inHome terminal' (spawn terminalCmd)
            ]
        lastTopics = [noAction (show i) "." | i <- [length firstTopics + 1 .. 10]]
     in firstTopics <> lastTopics

myTopicConfig :: TopicConfig
myTopicConfig =
    def
        { topicDirs = tiDirs topicItems
        , topicActions = tiActions topicItems
        , defaultTopicAction = const (pure ()) -- by default, do nothing
        , defaultTopic = web -- fallback
        }

scratchpads =
    []
  where
    chromePWA name appid = NS name ("google-chrome --new-window --app-id=" <> appid) (appName =? ("crx_" <> appid)) defaultFloating
    chromeWebapp name url appname = NS name ("google-chrome --new-window --app=" <> url) (appName =? appname) defaultFloating

myLayoutHook =
    Grid (16 / 9) ||| Full
        & desktopLayoutModifiers
        & smartSpacing 10
        & draggingVisualizer

myManageHook =
    mconcat $
        [ internetBrowser --> doShift web
        , className =? "emacs" --> doShift dev
        , jetbrainsIde --> doShift dev
        , chatRules --> doShift chat
        , className =? "Logseq" --> doShift notes
        ]
            <> zoomHooks
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
    chatRules =
        foldl1
            (<||>)
            [ className =? "Signal"
            , className =? "Slack"
            , className =? "discord"
            , className =? "thunderbird" -- TODO: named scratchpad
            ]
    zoomHooks =
        [ className =? "zoom" --> doShift call
        , (className =? "zoom") <&&> (stringProperty "WM_NAME" =? "") --> doFloat
        ]

myAfterRescreenHook = do
    setupWorkspaceGroups
    startBars

myStartupHook :: X ()
myStartupHook = do
    setupWorkspaceGroups
    fixJvmGuisNotWorking
    startNotificationDaemon
    startBars
    startRedshift
    startCompositionManager
    startTouchpadGesturesManager
    capslockIsCtrl
    setWallpaper
    setAutoScreenLock
    populateTray
    startPolkitAgent

setupWorkspaceGroups = do
    addRawWSGroup dev
        =<< [preset|
                chat | dev | web
                       dev | web
                       dev
            |]
    addRawWSGroup call
        =<< [preset|
                chat | call | web
                       call | web
                       call
            |]

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

startPolkitAgent = do
    spawnOnce "mate-polkit" -- Ubuntu
    spawnOnce "/usr/lib/mate-polkit/polkit-mate-authentication-agent-1" -- Arch

populateTray = do
    spawnOnce "nm-applet"
    spawnOnce "blueman-applet"
    spawnOnce "flameshot"

setAutoScreenLock = do
    spawnOnce idleHookCmd
