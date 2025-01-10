import XMonad
import qualified Data.Map as M
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.ManageHelpers
import XMonad.Hooks.StatusBar
import XMonad.Hooks.StatusBar.PP
import XMonad.Hooks.EwmhDesktops

import XMonad.StackSet as W

import XMonad.Util.EZConfig
import XMonad.Util.Loggers
import XMonad.Util.SpawnOnce

import XMonad.Layout.Magnifier
import XMonad.Layout.ThreeColumns
import XMonad.Layout.Renamed
import XMonad.Layout.WindowNavigation
import XMonad.Actions.Navigation2D

import Graphics.X11.ExtraTypes.XF86

-- Main configuration
main :: IO ()
main = xmonad
     . ewmhFullscreen
     . ewmh
     . withEasySB (statusBarProp "xmobar" (pure myXmobarPP)) defToggleStrutsKey
     $ myConfig

myConfig = def
    { modMask    = mod4Mask  -- Use Super key as the modifier
    , layoutHook = myLayout
    , manageHook = myManageHook
    , startupHook = myStartupHook
    , terminal = "alacritty"
    , keys = myKeys  -- Override default keys with custom keybindings
    }

-- Custom keybindings
myKeys :: XConfig Layout -> M.Map (KeyMask, KeySym) (X ())
myKeys conf@(XConfig {modMask = modm}) = M.fromList $
    [
      ((modm, xK_w), windowGo U False)
    , ((modm, xK_a), windowGo L False)
    , ((modm, xK_r), windowGo D False)
    , ((modm, xK_s), windowGo R False)

    , ((modm .|. shiftMask, xK_w), windowSwap U False)
    , ((modm .|. shiftMask, xK_a), windowSwap L False)
    , ((modm .|. shiftMask, xK_r), windowSwap D False)
    , ((modm .|. shiftMask, xK_s), windowSwap R False)

    , ((modm, xK_p), spawn "rofi -show drun")

    , ((0, xF86XK_AudioRaiseVolume), spawn "pactl set-sink-volume @DEFAULT_SINK@ +5%")
    , ((0, xF86XK_AudioLowerVolume), spawn "pactl set-sink-volume @DEFAULT_SINK@ -5%")
    , ((0, xF86XK_AudioMute), spawn "pactl set-sink-mute @DEFAULT_SINK@ toggle")

    , ((0, xF86XK_MonBrightnessUp), spawn "brightnessctl set +10%")
    , ((0, xF86XK_MonBrightnessDown), spawn "brightnessctl set 10%-")

    , ((modm, xK_1), windows $ W.greedyView "1")
    , ((modm, xK_2), windows $ W.greedyView "2")
    , ((modm, xK_3), windows $ W.greedyView "3")
    , ((modm, xK_4), windows $ W.greedyView "4")
    , ((modm, xK_5), windows $ W.greedyView "5")
    , ((modm, xK_6), windows $ W.greedyView "6")
    , ((modm, xK_7), windows $ W.greedyView "7")
    , ((modm, xK_8), windows $ W.greedyView "8")
    , ((modm, xK_9), windows $ W.greedyView "9")
    , ((modm, xK_0), windows $ W.greedyView "0")

    , ((modm .|. shiftMask, xK_1), windows $ W.shift "1")
    , ((modm .|. shiftMask, xK_2), windows $ W.shift "2")
    , ((modm .|. shiftMask, xK_3), windows $ W.shift "3")
    , ((modm .|. shiftMask, xK_4), windows $ W.shift "4")
    , ((modm .|. shiftMask, xK_5), windows $ W.shift "5")
    , ((modm .|. shiftMask, xK_6), windows $ W.shift "6")
    , ((modm .|. shiftMask, xK_7), windows $ W.shift "7")
    , ((modm .|. shiftMask, xK_8), windows $ W.shift "8")
    , ((modm .|. shiftMask, xK_9), windows $ W.shift "9")
    , ((modm .|. shiftMask, xK_0), windows $ W.shift "0")
    ]

myLayout = windowNavigation $ renamed [Replace "Tall"] tiled
       ||| windowNavigation (renamed [Replace "Mirror"] (Mirror tiled))
       ||| renamed [Replace "Full"] Full
       ||| windowNavigation (renamed [Replace "ThreeCol"] threeCol)
  where
    threeCol = magnifiercz' 1.3 $ ThreeColMid nmaster delta ratio
    tiled    = Tall nmaster delta ratio
    nmaster  = 1
    ratio    = 1/2
    delta    = 3/100

-- Window management
myManageHook :: ManageHook
myManageHook = composeAll
    [ className =? "Gimp" --> doFloat
    , isDialog            --> doFloat
    , className =? "Rofi" --> doFloat
    ]

-- Startup hook
myStartupHook :: X ()
myStartupHook = do
    spawn "setxkbmap -option caps:escape"
    spawnOnce "trayer --edge top --align right --SetDockType true --SetPartialStrut true \
             \--expand true --width 10 --transparent true --tint 0x282a36 --height 22 \
             \--distance 1 --distancefrom right --iconspacing 5 --alpha 0"
    spawnOnce "nm-applet"  -- Network Manager
    spawnOnce "pasystray"  -- PulseAudio/PipeWire control
    spawnOnce "blueman-applet"  -- Bluetooth
    spawnOnce "udiskie --tray"  -- USB disk automounter

-- XMobar configuration
myXmobarPP :: PP
myXmobarPP = def
    { ppSep             = magenta " • "
    , ppTitleSanitize   = xmobarStrip
    , ppCurrent         = wrap " " "" . xmobarBorder "Top" "#8be9fd" 2
    , ppHidden          = white . wrap " " ""
    , ppHiddenNoWindows = lowWhite . wrap " " ""
    , ppUrgent          = red . wrap (yellow "!") (yellow "!")
    , ppOrder           = \[ws, l, _, wins] -> [ws, l, wins]
    , ppExtras          = [logTitles formatFocused formatUnfocused]
    }
  where
    formatFocused   = wrap (white    "[") (white    "]") . magenta . ppWindow
    formatUnfocused = wrap (lowWhite "[") (lowWhite "]") . blue    . ppWindow

    ppWindow :: String -> String
    ppWindow = xmobarRaw . (\w -> if null w then "untitled" else w) . shorten 30

    blue, lowWhite, magenta, red, white, yellow :: String -> String
    magenta  = xmobarColor "#ff79c6" ""
    blue     = xmobarColor "#bd93f9" ""
    white    = xmobarColor "#f8f8f2" ""
    yellow   = xmobarColor "#f1fa8c" ""
    red      = xmobarColor "#ff5555" ""
    lowWhite = xmobarColor "#bbbbbb" ""
