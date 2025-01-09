import XMonad

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
:qmain = xmonad
     . ewmhFullscreen
     . ewmh
     . withEasySB (statusBarProp "xmobar" (pure myXmobarPP)) defToggleStrutsKey
     $ myConfig

myConfig = def
    { modMask    = mod4Mask      -- Rebind Mod to the Super key (Win)
    , layoutHook = myLayout      -- Use custom layouts
    , manageHook = myManageHook  -- Match on certain windows
    , startupHook = myStartupHook
    , terminal = "alacritty"     -- Default terminal
    }
  `additionalKeysP`
    [ -- Directional navigation (Colemak)
      ("M-w", windowGo U False)  -- Up
    , ("M-a", windowGo L False)  -- Left
    , ("M-r", windowGo D False)  -- Down
    , ("M-s", windowGo R False)  -- Right
    
    -- Window movement (Colemak)
    , ("M-S-n", windowSwap L False)  -- Left
    , ("M-S-e", windowSwap U False)  -- Up
    , ("M-S-i", windowSwap D False)  -- Down
    , ("M-S-o", windowSwap R False)  -- Right
    
    -- Applications
    , ("M-p", spawn "dmenu_run")  -- Application launcher
    , ("M-S-p", spawn "rofi -show window")  -- Window switcher
    
    -- Audio controls (using pipewire)
    , ("<XF86AudioRaiseVolume>", spawn "pactl set-sink-volume @DEFAULT_SINK@ +5%")
    , ("<XF86AudioLowerVolume>", spawn "pactl set-sink-volume @DEFAULT_SINK@ -5%")
    , ("<XF86AudioMute>", spawn "pactl set-sink-mute @DEFAULT_SINK@ toggle")
    
    -- Brightness controls
    , ("<XF86MonBrightnessUp>", spawn "brightnessctl set +10%")
    , ("<XF86MonBrightnessDown>", spawn "brightnessctl set 10%-")
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
    -- Set caps lock as escape
    spawn "setxkbmap -option caps:escape"
    -- Start system tray with better configuration
    spawnOnce "trayer --edge top --align right --SetDockType true --SetPartialStrut true \
             \--expand true --width 10 --transparent true --tint 0x282a36 --height 22 \
             \--distance 1 --distancefrom right --iconspacing 5 --alpha 0"
    -- Start common system tray applications
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

    -- Colors
    blue, lowWhite, magenta, red, white, yellow :: String -> String
    magenta  = xmobarColor "#ff79c6" ""
    blue     = xmobarColor "#bd93f9" ""
    white    = xmobarColor "#f8f8f2" ""
    yellow   = xmobarColor "#f1fa8c" ""
    red      = xmobarColor "#ff5555" ""
    lowWhite = xmobarColor "#bbbbbb" ""
