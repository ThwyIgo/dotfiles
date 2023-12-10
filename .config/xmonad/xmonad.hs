{- Dependencies:
  | acpilight  | nm-applet | playerctl |
  | alsa-utils |  trayer   |    rofi   |
-}

import XMonad
-- Make XMonad EWMH compliant. For example, this fixes full screen applications.
import XMonad.Hooks.EwmhDesktops
import Data.Monoid
import System.Exit
-- Emacs keybindings
import XMonad.Util.EZConfig
import XMonad.Util.Run
import XMonad.Util.SpawnOnce
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.ManageHelpers
import XMonad.Actions.CycleWS

-- XMobar
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.StatusBar
import XMonad.Hooks.StatusBar.PP
import XMonad.Util.Loggers
import XMonad.Layout.NoBorders

-- Layouts
import XMonad.Layout.Renamed
import XMonad.Layout.Combo
import XMonad.Layout.WindowNavigation
import XMonad.Layout.Tabbed
import XMonad.Layout.ResizableTile
import XMonad.Layout.Spacing
import XMonad.Layout.ToggleLayouts

import qualified XMonad.StackSet as W
import qualified Data.Map        as M

-- The preferred terminal program, which is used in a binding below and by
-- certain contrib modules.
--
myTerminal  = "alacritty"
fileManager = "nemo"
browser     = "librewolf"
emailClient = "thunderbird"
calculator  = "gnome-calculator"
screenshot  = "flameshot gui"

-- Whether focus follows the mouse pointer.
myFocusFollowsMouse :: Bool
myFocusFollowsMouse = True

-- Whether clicking on a window to focus also passes the click to the window
myClickJustFocuses :: Bool
myClickJustFocuses = False

-- Width of the window border in pixels.
--
myBorderWidth   = 1

-- modMask lets you specify which modkey you want to use. The default
-- is mod1Mask ("left alt").  You may also consider using mod3Mask
-- ("right alt"), which does not conflict with emacs keybindings. The
-- "windows key" is usually mod4Mask.
--
myModMask       = mod4Mask

-- The default number of workspaces (virtual screens) and their names.
-- By default we use numeric strings, but any string may be used as a
-- workspace name. The number of workspaces is determined by the length
-- of this list.
--
-- A tagging example:
--
-- > workspaces = ["web", "irc", "code" ] ++ map show [4..9]
--
myWorkspaces    = map show [1..9]

-- Border colors for unfocused and focused windows, respectively.
--
myNormalBorderColor  = "#dddddd"
myFocusedBorderColor = "#ff0000"

------------------------------------------------------------------------
-- Key bindings. Add, modify or remove key bindings here.
--
myKeys :: XConfig l -> [(String, X ())]
myKeys c =
    -- launch a terminal
    [ ("M-t", spawn $ terminal c)

    -- launch app launcher
    , ("M-<Return>", spawn "rofi -show-icons -show drun")

    -- close focused window
    , ("M-q", kill)

     -- Rotate through the available layout algorithms
    , ("M-<Tab>", sendMessage NextLayout)

    -- Toggle full screen
    , ("M-m", sendMessage $ Toggle "Full")

    , ("M-j",   windows W.focusDown)
    , ("M-k",   windows W.focusUp)
    , ("M-n",   windows W.swapMaster)
    , ("M-S-j", windows W.swapDown)
    , ("M-S-k", windows W.swapUp)

    -- WindowNavigation keybindings
    , ("M-<Right>",   sendMessage $ Go R)
    , ("M-<Left>",    sendMessage $ Go L)
    , ("M-<Up>",      sendMessage $ Go U)
    , ("M-<Down>",    sendMessage $ Go D)
    , ("M-S-<Right>", sendMessage $ Move R)
    , ("M-S-<Left>",  sendMessage $ Move L)
    , ("M-S-<Up>",    sendMessage $ Move U)
    , ("M-S-<Down>",  sendMessage $ Move D)

    -- Shrink the master area
    , ("M-h", sendMessage Shrink)

    -- Expand the master area
    , ("M-l", sendMessage Expand)

    -- Resize window (vertically)
    , ("M-C-j", sendMessage MirrorShrink)
    , ("M-C-k", sendMessage MirrorExpand)

    -- Push window back into tiling
    , ("M-f", withFocused $ windows . W.sink)

    -- Increment the number of windows in the master area
    , ("M-,", sendMessage (IncMasterN 1))

    -- Deincrement the number of windows in the master area
    , ("M-.", sendMessage (IncMasterN (-1)))

    -- Toggle the status bar gap
    -- Use this binding with avoidStruts from Hooks.ManageDocks.
    -- See also the statusBar function from Hooks.DynamicLog.
    --
    -- , ((modm              , xK_b     ), sendMessage ToggleStruts)

    -- Lock
    , ("M-<Pause>", spawn "slock")

    -- Quit xmonad
    , ("M-C-q", io exitSuccess)

    -- Quit GUI
    , ("M-<Escape>", spawn "session-quit")

    -- Restart xmonad
    , ("M-C-r", spawn "xmonad --recompile && xmonad --restart")
    ]
    ++

    -- Launch apps
    [ ("M-a " ++ key, spawn cmd)
    | (key, cmd) <- [ ("e", "emacsclient --alternate-editor=emacs -c")
                    , ("c", calculator)
                    , ("f", fileManager)
                    , ("v", "pavucontrol")
                    , ("b", browser)
                    , ("m", emailClient)
                    ]
    ]
    ++
    [ ("M-p", spawn "arandr") ]
    ++

    -- Controls, special keys
    --
    [ ("<Print>", spawn screenshot)
    , ("<XF86MonBrightnessUp>"  , spawn "xbacklight -inc 5")
    , ("<XF86MonBrightnessDown>", spawn "xbacklight -dec 5")
    , ("<XF86AudioLowerVolume>" , spawn "amixer set Master 10%-")
    , ("<XF86AudioRaiseVolume>" , spawn "amixer set Master 10%+")
    , ("<XF86AudioMute>", spawn "amixer set Master toggle")
    , ("<XF86AudioPlay>", spawn "playerctl play-pause")
    , ("<XF86AudioPrev>", spawn "playerctl previous")
    , ("<XF86AudioNext>", spawn "playerctl next")
    ]
    ++

    -- mod-[1..9], Switch to workspace N
    -- mod-shift-[1..9], Move client to workspace N
    -- mod-0, Switch to next workspace
    -- mod-ctrl-0, Switch to previous workspace
    --
    [ ("M-" ++ mkey ++ num, windows $ action wsName)
    | (mkey, action) <- [([], W.greedyView), ("S-", W.shift)],
      (num, wsName)  <- zip (map show [1..9]) (XMonad.workspaces c)
    ]
    ++
    [ ("M-0", nextWS)
    , ("M-C-0", prevWS)
    , ("M-S-0", shiftToNext)
    , ("M-C-S-0", shiftToPrev)
    ]
    ++

    -- mod-{z,x,c}, Switch to physical/Xinerama screens 1, 2, or 3
    -- mod-shift-{z,x,c}, Move client to screen 1, 2, or 3
    --
    [ ("M-" ++ mkey ++ key, screenWorkspace sc >>= flip whenJust (windows . action))
    | (mkey, action) <- [([], W.view), ("S-", W.shift)],
      (key, sc)  <- zip ["z","x","c"] [0..]
    ]


------------------------------------------------------------------------
-- Mouse bindings: default actions bound to mouse events
--
myMouseBindings (XConfig {XMonad.modMask = modm}) = M.fromList $

    -- mod-button1, Set the window to floating mode and move by dragging
    [ ((modm, button1), \w -> focus w >> mouseMoveWindow w
                              >> windows W.shiftMaster)

    -- mod-button2, Raise the window to the top of the stack
    , ((modm, button2), \w -> focus w >> windows W.shiftMaster)

    -- mod-button3, Set the window to floating mode and resize by dragging
    , ((modm, button3), \w -> focus w >> mouseResizeWindow w
                              >> windows W.shiftMaster)

    -- you may also bind events to the mouse scroll wheel (button4 and button5)
    ]

------------------------------------------------------------------------
-- Layouts:

-- You can specify and transform your layouts by modifying these values.
-- If you change layout bindings be sure to use 'mod-shift-space' after
-- restarting (with 'mod-q') to reset your layout state to the new
-- defaults, as xmonad preserves your old layout settings by default.
--
-- The available layouts.  Note that each layout is separated by |||,
-- which denotes layout choice.
--
-- Gaps
mySpacing i = spacingRaw True (Border i i i i) True (Border i i i i) True

myLayout = toggleLayouts (noBorders Full) $ windowNavigation . smartBorders . avoidStruts $
  (tiled ||| noBorders tabs ||| combo)
  where
     tiled   = renamed [Replace "Tall"] $
               mySpacing 5 $ ResizableTall nmaster delta ratio []
     -- The default number of windows in the master pane
     nmaster = 1
     -- Default proportion of screen occupied by master pane
     ratio   = 1/2
     -- Percent of screen to increment by when resizing panes
     delta   = 3/100

     tabs = renamed [Replace "Tabbed"] $
            tabbed shrinkText myTabConfig
     myTabConfig = def { activeColor         = "#44475A"
                       , inactiveColor       = "#282A36"
                       , activeBorderColor   = "#BD93F9"
                       , inactiveBorderColor = "#6272A4"
                       , activeTextColor     = "#F8F8F2" }

     combo = renamed [Replace "Combo"] $
             combineTwo tiled tabs tabs

------------------------------------------------------------------------
-- Window rules:

-- Execute arbitrary actions and WindowSet manipulations when managing
-- a new window. You can use this to, for example, always float a
-- particular program, or have a client always appear on a particular
-- workspace.
--
-- To find the property name associated with a program, use
-- > xprop | grep WM_CLASS
-- and click on the client you're interested in.
--
-- To match on the WM_NAME, you can use 'title' in the same way that
-- 'className' and 'resource' are used below.
--
myManageHook = composeAll
    [ className =? "MPlayer"          --> doFloat
    , className =? "Gimp"             --> doFloat
    , className =? "Gnome-screenshot" --> doFloat
    , className =? "gnome-calculator" --> doFloat -- Not working
    , title   =? "Picture-in-Picture" --> doFloat
    , resource  =? "desktop_window"   --> doIgnore
    , resource  =? "kdesktop"         --> doIgnore
    , isDialog                        --> doFloat

    , className =? "KeePassXC" --> doShift (myWorkspaces !! 5)
    , className =? "Spotify"   --> doShift (myWorkspaces !! 4) ]

------------------------------------------------------------------------
-- Event handling

-- * EwmhDesktops users should change this to ewmhDesktopsEventHook
--
-- Defines a custom handler function for X Events. The function should
-- return (All True) if the default handler is to be run afterwards. To
-- combine event hooks use mappend or mconcat from Data.Monoid.
--
myEventHook = mempty

------------------------------------------------------------------------
-- Status bars and logging

-- Perform an arbitrary action on each internal state change or X event.
-- See the 'XMonad.Hooks.DynamicLog' extension for examples.
--
myLogHook = return ()

------------------------------------------------------------------------
-- Startup hook

-- Perform an arbitrary action each time xmonad starts or is restarted.
-- Used by, e.g., XMonad.Layout.PerWorkspace to initialize
-- per-workspace layout choices.
--
myStartupHook = do
  -- Don't delete the return ()
  return () >> checkKeymap defaults (myKeys defaults)
  spawnOnce "trayer --edge top --align right --SetDockType true --SetPartialStrut true --expand true --widthtype request --transparent true --alpha 0 --tint 0x000000 --height 25 --iconspacing 1"
  spawnOnce "nm-applet --sm-disable"
  spawnOnce "feh --bg-scale --randomize ~/.local/share/wallpapers/**"
  spawnOnce "picom -b --experimental-backends"
  spawnOnce "xsetroot -cursor_name left_ptr"
  {- XMobar crashes if it starts before (alsa plugin + wireplumber).
     Restarting XMonad after wireplumber (hopefully) fixes the problem.
   -}
  spawnOnce $ unwords [ "sh -c 'if [ -z $(pgrep wireplumber) ];"
                      , "then until pgrep wireplumber; do"
                      ,   "sleep 1;"
                      ,   "done && sleep 1 && xmonad --restart;"
                      , "fi'"
                      ]

------------------------------------------------------------------------
-- XMobar Prop

-- How should XMonad interact with XMobar
-- Default: xmobarProp
--
myXmobarProp = withEasySB (statusBarProp ("xmobar " ++ rcPath) (pure myXmobarPP)) defToggleStrutsKey
  where
    rcPath = "~/.config/xmobar/xmobarrc"
    myXmobarPP = def
      { ppSep           = " | "
      , ppTitleSanitize = xmobarStrip
      , ppTitle         = white . shorten 35
      , ppCurrent       = magenta . wrap "[" "]"
      , ppOrder         = \[ws,l,wt] -> [l,ws,wt]
      , ppLayout        = (' ':)
      }

------------------------------------------------------------------------
-- Now run xmonad with all the defaults we set up.

-- Run xmonad with the settings you specify. No need to modify this.
--
main = xmonad . ewmhFullscreen . ewmh . myXmobarProp $ docks defaults

-- XMobar colors           fg    bg
magenta  = xmobarColor "#913bbf" ""
blue     = xmobarColor "#bd93f9" ""
white    = xmobarColor "#f8f8f2" ""
yellow   = xmobarColor "#f1fa8c" ""
red      = xmobarColor "#ff5555" ""
lowWhite = xmobarColor "#bbbbbb" ""

-- A structure containing your configuration settings, overriding
-- fields in the default config. Any you don't override, will
-- use the defaults defined in xmonad/XMonad/Config.hs
--
-- No need to modify this.
--
--defaults :: XConfig Layout
defaults = def {
      -- simple stuff
        terminal           = myTerminal,
        focusFollowsMouse  = myFocusFollowsMouse,
        clickJustFocuses   = myClickJustFocuses,
        borderWidth        = myBorderWidth,
        modMask            = myModMask,
        workspaces         = myWorkspaces,
        normalBorderColor  = myNormalBorderColor,
        focusedBorderColor = myFocusedBorderColor,

      -- key bindings
        keys               = \c -> mkKeymap c (myKeys c),
        mouseBindings      = myMouseBindings,

      -- hooks, layouts
        layoutHook         = myLayout,
        manageHook         = myManageHook,
        handleEventHook    = myEventHook,
        logHook            = myLogHook,
        startupHook        = myStartupHook
    }
