-- Osiris's XMonad config

import System.Exit
import Data.Maybe (Maybe, isNothing, fromJust)
import qualified Data.List as L
import qualified Data.Map as M
import GHC.IO.Handle
import Graphics.X11.ExtraTypes.XF86

-- Xmonad Core
import XMonad
import qualified XMonad.StackSet as W
import XMonad.Config.Desktop

-- Layouts
import XMonad.Layout.LayoutModifier
import XMonad.Layout.Gaps
import XMonad.Layout.Spacing
import XMonad.Layout.MultiToggle
import XMonad.Layout.NoBorders
import XMonad.Layout.MultiToggle.Instances
import XMonad.Layout.ResizableTile
import XMonad.Layout.SimpleFloat
import XMonad.Layout.BinarySpacePartition
import XMonad.Layout.PerWorkspace (onWorkspace)
import XMonad.Layout.Minimize
import XMonad.Layout.Named
import XMonad.Layout.Tabbed
import XMonad.Layout.ThreeColumns
import XMonad.Layout.MultiToggle.Instances
import XMonad.Layout.NoFrillsDecoration
import XMonad.Layout.Renamed
import XMonad.Layout.Simplest
import XMonad.Layout.SubLayouts
import XMonad.Layout.WindowNavigation
import XMonad.Layout.ZoomRow

-- Actions
import XMonad.Actions.Navigation2D
import XMonad.Actions.GridSelect
import XMonad.Actions.UpdatePointer
import XMonad.Actions.SpawnOn
import XMonad.Actions.CycleWS

-- Hooks
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageHelpers
import XMonad.Hooks.SetWMName
import XMonad.Hooks.EwmhDesktops
import XMonad.Hooks.ManageDocks

-- Utils
import XMonad.Util.NamedScratchpad
import XMonad.Util.WorkspaceCompare
import XMonad.Util.Run
import XMonad.Util.EZConfig
import XMonad.Util.SpawnOnce

----------------------------mupdf--------------------------------------------
-- Terminal
-- The preferred terminal program, which is used in a binding below and by
-- certain contrib modules.
--
myTerminal = "urxvt"

-- The command to lock the screen or show the screensaver.
myScreensaver = "xscreensaver-command -lock"

-- The command to use as a launcher, to launch commands that don't have
-- preset keybindings.
myLauncher = "rofi -show run"

-- The command to use as a window manage
myWindowManager = "rofi -show window"

------------------------------------------------------------------------
-- Workspaces
-- The default number of workspaces (virtual screens) and their names.
--
myWorkspaces = ["1:web","2:term","3:code","4:vm","5:media", "6:chat"] ++ map show [7..8] ++ ["NSP"]


------------------------------------------------------------------------
-- Window rules
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
    [
    className =? "Google-chrome"               --> doShift "1:web"
    , className =? "Firefox"                   --> doShift "1:web"
    , resource  =? "desktop_window"            --> doIgnore
    , className =? "Steam"                     --> doCenterFloat
    , className =? "Vlc"                       --> doShift "5:media" <+> doCenterFloat
    , className =? "TelegramDesktop"           --> doShift "6:chat"--(customFloating $ W.RationalRect (1/6) (1/6) (2/3) (2/3))
    , className =? "Slack"                     --> doShift "6:chat"
    , className =? "VirtualBox"                --> doShift "4:vm"
    , className =? "spotify"                   --> doShift "5:media"
    , className =? "stalonetray"               --> doIgnore
    , isFullscreen                             --> doFullFloat
    -- , isFullscreen                             --> (doF W.focusDown <+> doFullFloat)
    ]

-- Function that prevents cycling to workspaces available on other screens
hiddenNotNSP :: X (WindowSpace -> Bool)
hiddenNotNSP = do
  hs <- gets $ map W.tag . W.hidden . windowset
  return (\w -> (W.tag w) /= "NSP" && (W.tag w) `elem` hs)

------------------------------------------------------------------------
-- Layouts
-- You can specify and transform your layouts by modifying these values.
-- If you change layout bindings be sure to use 'mod-shift-space' after
-- restarting (with 'mod-q') to reset your layout state to the new
-- defaults, as xmonad preserves your old layout settings by default.
--
-- The available layouts.  Note that each layout is separated by |||,
-- which denotes layout choice.

outerGaps = 0
myGaps = gaps [(U, outerGaps), (R, outerGaps), (L, outerGaps), (D, outerGaps)]
addSpace = renamed [CutWordsLeft 2] . spacing gap
tab                  =  avoidStruts
                       $ addTopBar
                       $ myGaps
                       $ renamed [Replace "Tabbed"]
                       $ tabbed shrinkText myTabTheme

layouts              = avoidStruts (
                        (
                            addTopBar
                          $ windowNavigation
                          $ renamed [CutWordsLeft 1]
                          $ addTabs shrinkText myTabTheme
                          $ subLayout [] Simplest
                          $ myGaps
                          $ addSpace (emptyBSP ||| ThreeColMid 1 (3/100) (1/2) ||| Full)
                        )
                        ||| tab
                       )

myLayout = smartBorders
           $ renamed [CutWordsLeft 1]
           $ minimize
           $ mkToggle (NOBORDERS ?? FULL ?? EOT)
           $ layouts

myNav2DConf = def
    { defaultTiledNavigation    = centerNavigation
    , floatNavigation           = centerNavigation
    , screenNavigation          = lineNavigation
    }


------------------------------------------------------------------------
-- Colors and borders

-- Color of current window title in xmobar.
xmobarTitleColor = "#FFB6B0"

-- Color of current workspace in xmobar.
xmobarCurrentWorkspaceColor = "#CEFFAC"

-- Width of the window border in pixels.
myBorderWidth = 0

myNormalBorderColor     = "#000000"
myFocusedBorderColor    = active

base03  = "#002b36"
base02  = "#073642"
base01  = "#586e75"
base00  = "#657b83"
base0   = "#839496"
base1   = "#93a1a1"
base2   = "#eee8d5"
base3   = "#fdf6e3"
yellow  = "#b58900"
orange  = "#cb4b16"
red     = "#dc322f"
magenta = "#d33682"
violet  = "#6c71c4"
blue    = "#268bd2"
cyan    = "#2aa198"
green   = "#859900"

-- sizes
gap         = 1 
topbar      = 2 
border      = 0 
prompt      = 20
status      = 20

active      = cyan
activeWarn  = red
inactive    = base02
focusColor  = cyan
unfocusColor = base02

-- myFont      = "-*-terminus-medium-*-*-*-*-160-*-*-*-*-*-*"
-- myBigFont   = "-*-terminus-medium-*-*-*-*-240-*-*-*-*-*-*"
myFont      = "-*-SourceCodePro Nerd Font-medium-*-*-*-*-160-*-*-*-*-*-*"
myBigFont   = "-*-SourceCodePro Nerd Fon-medium-*-*-*-*-240-*-*-*-*-*-*"
myWideFont  = "xft:Eurostar Black Extended:"
            ++ "style=Regular:pixelsize=180:hinting=true"

-- this is a "fake title" used as a highlight bar in lieu of full borders
-- (I find this a cleaner and less visually intrusive solution)
topBarTheme = def
    {
      fontName              = myFont
    , inactiveBorderColor   = base03
    , inactiveColor         = base03
    , inactiveTextColor     = base03
    , activeBorderColor     = active
    , activeColor           = active
    , activeTextColor       = active
    , urgentBorderColor     = red
    , urgentTextColor       = yellow
    , decoHeight            = topbar
    }

addTopBar = renamed [CutWordsLeft 1] . noFrillsDeco shrinkText topBarTheme

myTabTheme = def
    { fontName              = myFont
    , activeColor           = active
    , inactiveColor         = base02
    , activeBorderColor     = active
    , inactiveBorderColor   = base02
    , activeTextColor       = base03
    , inactiveTextColor     = base00
    }


------------------------------------------------------------------------
-- Key bindings
--
-- modMask lets you specify which modkey you want to use. The default
-- is mod1Mask ("left alt").  You may also consider using mod3Mask
-- ("right alt"), which does not conflict with emacs keybindings. The
-- "windows key" is usually mod4Mask.
--
myModMask = mod4Mask
altMask = mod1Mask

-- Key bindings. Add, modify or remove key bindings here.
myKeys conf@(XConfig {XMonad.modMask = modm}) = M.fromList $

    -- launch a terminal
    [ ((modm .|. shiftMask,  xK_Return), spawn $ XMonad.terminal conf)

    -- launch file manager
    , ((modm .|. shiftMask, xK_f     ), spawn "thunar")

    -- launch roficlip
    , ((modm,           xK_c         ), spawn "roficlip")
    , ((modm,           xK_backslash ), spawn "roficlip")

    -- launch rofi
    , ((modm,               xK_p     ), spawn myLauncher)
    , ((modm .|. shiftMask, xK_p     ), spawn myWindowManager)

    -- launch telegram
    , ((modm,               xK_F10   ), spawn "telegram-desktop")

      -- Mute volume.
    , ((0,           xF86XK_AudioMute), spawn "amixer -q set Master toggle")
    -- Decrease volume.
    , ((0,    xF86XK_AudioLowerVolume), spawn "amixer -q set Master 5%-")
    -- Increase volume.
    , ((0,    xF86XK_AudioRaiseVolume), spawn "amixer -q set Master 5%+")

    -- close focused window
    , ((modm .|. shiftMask, xK_c     ), kill)

     -- Rotate through the available layout algorithms
    , ((modm,               xK_space ), sendMessage NextLayout)

    --  Reset the layouts on the current workspace to default
    , ((modm .|. shiftMask, xK_space ), setLayout $ XMonad.layoutHook conf)

    -- Resize viewed windows to the correct size
    , ((modm .|. shiftMask, xK_r     ), refresh)

    -- Move focus to the next window
    , ((modm,               xK_Tab   ), windows W.focusDown)

    -- Move to previous workspace
    , ((modm,               xK_grave ), toggleWS' ["NSP"])

    -- Move focus to the next window
    , ((modm .|. controlMask .|. shiftMask,  xK_j     ), windows W.focusDown)

    -- Move focus to the previous window
    , ((modm .|. controlMask .|. shiftMask,  xK_k     ), windows W.focusUp)

    -- Minimize selected window
    , ((modm,               xK_m     ), withFocused minimizeWindow)

    -- Restore one minimized window
    , ((modm .|. shiftMask, xK_m     ), sendMessage RestoreNextMinimizedWin)

    -- Maximize selected window
    , ((modm,                           xK_f     ), (sendMessage $ Toggle FULL))

    -- Swap the focused window and the master window
    , ((modm .|. controlMask, xK_Return), windows W.swapMaster)

    -- Move focus to the master window
    , ((modm,                 xK_Return     ), windows W.focusMaster  )

    -- Swap the focused window with the next window
    , ((modm .|. controlMask .|. shiftMask, xK_j     ), windows W.swapDown  )

    -- Swap the focused window with the previous window
    , ((modm .|. controlMask .|. shiftMask, xK_k     ), windows W.swapUp    )

    -- Shrink the master area
    , ((modm .|. controlMask,               xK_h     ), sendMessage Shrink)
    , ((modm .|. controlMask,               xK_j     ), sendMessage MirrorShrink)

    -- Expand the master area
    , ((modm .|. controlMask,               xK_l     ), sendMessage Expand)
    , ((modm .|. controlMask,               xK_k     ), sendMessage MirrorExpand)

    -- Toggle Brightness
    , ((modm,               xK_minus ), spawn "xbacklight -dec 5")
    , ((modm,               xK_equal ), spawn "xbacklight -inc 5")

    -- Push window back into tiling
    , ((modm,               xK_t     ), withFocused $ windows . W.sink)

    -- Increment the number of windows in the master area
    , ((modm              , xK_comma ), sendMessage (IncMasterN 1))

    -- Decrement the number of windows in the master area
    , ((modm              , xK_period), sendMessage (IncMasterN (-1)))


    , ((modm              , xK_BackSpace), swapNextScreen)

    -- Switch workspaces and screens
    , ((modm,               xK_Right),  moveTo Next (WSIs hiddenNotNSP))
    , ((modm,               xK_Left),   moveTo Prev (WSIs hiddenNotNSP))
    , ((modm .|. shiftMask, xK_Right),  shiftTo Next (WSIs hiddenNotNSP))
    , ((modm .|. shiftMask, xK_Left),   shiftTo Prev (WSIs hiddenNotNSP))
    , ((modm,               xK_Down),   nextScreen)
    , ((modm,               xK_Up),     prevScreen)
    , ((modm .|. shiftMask, xK_Down),   shiftNextScreen)
    , ((modm .|. shiftMask, xK_Up),     shiftPrevScreen)

    -- Binary Space Partition Functions
    , ((modm .|. altMask,                  xK_l     ), sendMessage $ ExpandTowards R)
    , ((modm .|. altMask,                  xK_h     ), sendMessage $ ExpandTowards L)
    , ((modm .|. altMask,                  xK_j     ), sendMessage $ ExpandTowards D)
    , ((modm .|. altMask,                  xK_k     ), sendMessage $ ExpandTowards U)
    , ((modm .|. altMask .|. shiftMask,    xK_l     ), sendMessage $ ShrinkFrom R)
    , ((modm .|. altMask .|. shiftMask,    xK_h     ), sendMessage $ ShrinkFrom L)
    , ((modm .|. altMask .|. shiftMask,    xK_j     ), sendMessage $ ShrinkFrom D)
    , ((modm .|. altMask .|. shiftMask,    xK_k     ), sendMessage $ ShrinkFrom U)
    , ((modm,                              xK_d     ), sendMessage Rotate)

    -- Directional Navigation & Moving of Windows
   , ((modm,               xK_l), windowGo R False)
   , ((modm,               xK_h), windowGo L False)
   , ((modm,               xK_k), windowGo U False)
   , ((modm,               xK_j), windowGo D False)
   , ((modm .|. shiftMask, xK_l), windowSwap R False)
   , ((modm .|. shiftMask, xK_h), windowSwap L False)
   , ((modm .|. shiftMask, xK_k), windowSwap U False)
   , ((modm .|. shiftMask, xK_j), windowSwap D False)

    -- Toggle the status bar gap
    , ((modm              , xK_b     ), sendMessage ToggleStruts)

    -- Quit xmonad
    , ((modm .|. shiftMask, xK_q     ), io (exitWith ExitSuccess))

    -- Restart xmonad
    , ((modm              , xK_q     ), spawn "xmonad --recompile; xmonad --restart")
    ]
    ++

    -- mod-[1..9], Switch to workspace N
    -- mod-shift-[1..9], Move client to workspace N
    [((m .|. modm, k), windows $ f i)
        | (i, k) <- zip (XMonad.workspaces conf) [xK_1 .. xK_9]
        , (f, m) <- [(W.greedyView, 0), (W.shift, shiftMask)]]
    ++

    -- mod-{w,e,r}, Switch to physical/Xinerama screens 1, 2, or 3
    -- mod-shift-{w,e,r}, Move client to screen 1, 2, or 3
    [((m .|. modm, key), screenWorkspace sc >>= flip whenJust (windows . f))
        | (key, sc) <- zip [xK_w, xK_e, xK_r] [0..]
        , (f, m) <- [(W.view, 0), (W.shift, shiftMask)]]


------------------------------------------------------------------------
-- Mouse bindings
--
-- Focus rules
-- True if your focus should follow your mouse cursor.
myFocusFollowsMouse :: Bool
myFocusFollowsMouse = True

myMouseBindings (XConfig {XMonad.modMask = modMask}) = M.fromList $
  [
    -- mod-button1, Set the window to floating mode and move by dragging
    ((modMask, button1),
     (\w -> focus w >> mouseMoveWindow w))

    -- mod-button2, Raise the window to the top of the stack
    , ((modMask, button2),
       (\w -> focus w >> windows W.swapMaster))

    -- mod-button3, Set the window to floating mode and resize by dragging
    , ((modMask, button3),
       (\w -> focus w >> mouseResizeWindow w))

    -- you may also bind events to the mouse scroll wheel (button4 and button5)
  ]


-----------------------------------------------------------------------
-- Handle event hook

-- By default empty

myEventHook = mempty

------------------------------------------------------------------------
-- Startup hook
-- Perform an arbitrary action each time xmonad starts or is restarted
-- with mod-q.  Used by, e.g., XMonad.Layout.PerWorkspace to initialize
-- per-workspace layout choices.
--
myStartupHook = do 
  spawnOnce "bash ~/.xmonad/startup.sh"

------------------------------------------------------------------------
-- Run xmonad with all the defaults we set up.
--
main = do
  xmproc <- spawnPipe "xmobar"
  xmonad $ docks
         $ withNavigation2DConfig myNav2DConf
         $ additionalNav2DKeys (xK_Up, xK_Left, xK_Down, xK_Right)
                               [
                                   (mod4Mask,               windowGo  )
                                 , (mod4Mask .|. shiftMask, windowSwap)
                               ]
                               False
         $ defaults {
         logHook = dynamicLogWithPP $ xmobarPP {
               ppOutput = hPutStrLn xmproc
             , ppTitle = xmobarColor xmobarTitleColor "" . shorten 70
             , ppCurrent = xmobarColor xmobarCurrentWorkspaceColor ""
             , ppSep = "   "
         }
      }

------------------------------------------------------------------------
-- Combine it all together
-- A structure containing your configuration settings, overriding
-- fields in the default config. Any you don't override, will
-- use the defaults defined in xmonad/XMonad/Config.hs
--
-- No need to modify this.
--
defaults = def {
    -- simple stuff
    terminal           = myTerminal,
    focusFollowsMouse  = myFocusFollowsMouse,
    borderWidth        = myBorderWidth,
    modMask            = myModMask,
    workspaces         = myWorkspaces,
    normalBorderColor  = myNormalBorderColor,
    focusedBorderColor = myFocusedBorderColor,

    -- key bindings
    keys               = myKeys,
    mouseBindings      = myMouseBindings,

    -- hooks, layouts
    layoutHook         = myLayout,
    handleEventHook    = myEventHook <+> handleEventHook desktopConfig,
    manageHook         = manageDocks <+> myManageHook,
    startupHook        = myStartupHook
}
