import XMonad
import XMonad.Layout.Spacing
import XMonad.Layout.Gaps
import XMonad.Layout.ToggleLayouts
import XMonad.Hooks.DynamicLog
import XMonad.Util.Run
import XMonad.Hooks.ManageDocks
import XMonad.Util.EZConfig
import XMonad.Layout.MultiToggle
import XMonad.Layout.NoBorders
import XMonad.Layout.MultiToggle.Instances
import XMonad.Hooks.SetWMName
import XMonad.Layout.Minimize
import XMonad.Util.Paste

import System.Exit
import Control.Concurrent.MVar
import qualified XMonad.StackSet as W
import qualified Data.Map as M

myTerminal = "urxvt"

myFocusFollowsMouse = True

-- Whether clicking on a window to focus also passes the click to the window
-- myClickJustFocuses :: Bool
-- myClickJustFocuses = False

myBorderWidth = 4

myModMask = mod1Mask

myWorkspaces = ["1: CODE","2: WEB","3: SYS","4: COMM","5: ETC"] ++ map show [6..9]

myNormalBorderColor  = "#222226"

myFocusedBorderColor = "#5d4d7a"

myXmobarSelectColor  = "#ce537a"

myXmobarWindowColor  = "#bc6ec5"

myKeys conf =
  [ ("M-<Return>", spawn (terminal conf))
  , ("M-r",   spawn "rofi -show run")
  , ("M-f",   spawn"firefox")
  , ("M-e",   spawn "emacs")
  , ("<XF86MonBrightnessDown>", spawn "xbacklight -dec 10")
  , ("<XF86MonBrightnessUp>"  , spawn "xbacklight -inc 10")
  , ("<XF86AudioLowerVolume>" , spawn "amixer set Master 5%-")
  , ("<XF86AudioRaiseVolume>" , spawn "amixer set Master 5%+")
  , ("M-j",   windows W.focusDown)
  , ("M-S-j", windows W.swapDown)
  , ("M-k",   windows W.focusUp)
  , ("M-S-k", windows W.swapUp)
  , ("M-m",   windows W.focusMaster)
  , ("M-p",   pasteSelection)
  , ("M-n",   sendMessage NextLayout)
  , ("M-c",   kill)
  , ("M-S-r", spawn "xmonad --recompile; xmonad --restart")
  , ("M-S-q", io exitSuccess)
  ] ++
  [("M"++ shf ++ "-" ++ [wsId], windows (f ws))
  | (ws,wsId) <- zip (workspaces conf) "&[{}(=*)+]"
  , (f ,shf) <- [(W.greedyView,""),(\w -> W.greedyView w . W.shift w ,"-S")]]

myMouseBindings (XConfig {XMonad.modMask = modm}) = M.empty

myLayout = avoidStruts (mkToggle (NOBORDERS ?? FULL ?? EOT) (tiled ||| Full))
  where
     -- default tiling algorithm partitions the screen into two panes
     tiled   = gaps [(U,10), (R,10), (L,10), (R,10)] $ spacing 10 $ Tall nmaster delta ratio

     -- The default number of windows in the master pane
     nmaster = 1

     -- Default proportion of screen occupied by master pane
     ratio   = 1/2

     -- Percent of screen to increment by when resizing panes
     delta   = 3/100

myManageHook = manageDocks

myEventHook = mempty

myLogHook xmproc = dynamicLogWithPP xmobarPP
                     { ppOutput = hPutStrLn xmproc
                     , ppCurrent = xmobarColor myXmobarWindowColor "" . wrap "[" "]"
                     , ppTitle = xmobarColor myXmobarSelectColor "" . shorten 50
                     }

myStartupHook conf= do
  return () --fixpoint of the startupHook, layoutHook loop
  checkKeymap conf (myKeys conf)

myConfig xmproc = def {
        terminal           = myTerminal,
        focusFollowsMouse  = myFocusFollowsMouse,
        --clickJustFocuses   = myClickJustFocuses,
        borderWidth        = myBorderWidth,
        modMask            = myModMask,
        workspaces         = myWorkspaces,
        normalBorderColor  = myNormalBorderColor,
        focusedBorderColor = myFocusedBorderColor,
        keys               = \c -> mkKeymap c (myKeys c),
        mouseBindings      = myMouseBindings,
        layoutHook         = myLayout,
        manageHook         = myManageHook,
        handleEventHook    = myEventHook,
        logHook            = myLogHook xmproc,
        startupHook        = myStartupHook (myConfig xmproc)
    }


main :: IO ()
main = do
  xmproc <- spawnPipe "/home/kyle/.local/bin/xmobar /home/kyle/.xmobarrc"
  xmonad $ myConfig xmproc
