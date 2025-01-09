import XMonad
import qualified Data.Map as M  -- Import Data.Map for defining keybindings

import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.ManageHelpers
import XMonad.Hooks.StatusBar
import XMonad.Hooks.StatusBar.PP
import XMonad.Hooks.EwmhDesktops

import XMonad.Util.EZConfig
import XMonad.Util.Loggers
import XMonad.Util.SpawnOnce

import XMonad.Layout.Magnifier
import XMonad.Layout.ThreeColumns
import XMonad.Layout.Renamed
import XMonad.Layout.WindowNavigation
import XMonad.Actions.Navigation2D

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
      ((modm, xK_w), windowGo U False)  -- Move focus up
    , ((modm, xK_a), windowGo L False)  -- Move focus left
    , ((modm, xK_r), windowGo D False)  -- Move focus down
    , ((modm, xK_s), windowGo R False)  -- Move focus right

    , ((modm .|. shiftMask, xK_w), windowSwap U False)  -- Swap window up
    , ((modm .|. shiftMask, xK_a), windowSwap L False)  -- Swap window left
    , ((modm .|. shiftMask, xK_r), windowSwap D False)  -- Swap window down
    , ((modm .|. shiftMask, xK_s), windowSwap R False)  -- Swap window right

    , ((modm, xK_p), spawn "rofi -show drun")  -- Application launcher
    , ((modm .|. shiftMask, xK_p), spawn "rofi -show window")  -- Window switcher

    -- , ((0, xF86XK_AudioRaiseVolume), spawn "pactl set-sink-volume @DEFAULT_SINK@ +5%")
    -- , ((0, xF86XK_AudioLowerVolume), spawn "pactl set-sink-volume @DEFAULT_SINK@ -5%")
    -- , ((0, xF86XK_AudioMute), spawn "pactl set-sink-mute @DEFAULT_SINK@ toggle")

    -- , ((0, xF86XK_MonBrightnessUp), spawn "brightnessctl set +10%")
    -- , ((0, xF86XK_MonBrightnessDown), spawn "brightnessctl set 10%-")
    ]

-- Layout configuration
myLayout = windowNavigation $ renamed [Replace "Tall"] tiled
       ||| windowNavigation (renamed [Replace "Mirror"] (Mirror tiled))
       ||| renamed [Replace "Full"] Full
       ||| windowNavigation (renamed [Replace "ThreeCol"] threeCol)
  where
    threeCol = magnifiercz' 1.3 $ ThreeColMid nmaster delta ratio
    tiled    = Tall nmaster delta ratio
    nmaster  = 1      -- Default number of windows in the master pane
    ratio    = 1/2    -- Default proportion of screen occupied by master pane
    delta    = 3/100  -- Percent of screen to increment by when resizing panes

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
