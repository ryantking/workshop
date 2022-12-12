module XMonad.Custom.Scratchpads
  ( myScratchPads
  ) where
import           XMonad
import           XMonad.Custom.Variables
import qualified XMonad.StackSet               as W
import           XMonad.Util.NamedScratchpad

myScratchPads :: [NamedScratchpad]
myScratchPads =
  [ NS "terminal"
       (myTerminal ++ " -c tscratchpad")
       (className =? "tscratchpad")
       manageTerm
  , NS "calculator" "qalculate-gtk" (className =? "Qalculate-gtk") manageWindow
  , NS "pavu" "pavucontrol-qt" (className =? "pavucontrol-qt") manageWindow
  , NS "networkmanager"
       "nm-connection-editor"
       (className =? "Nm-connection-editor")
       manageWindow
  , NS "bluetooth"
       "blueman-manager"
       (className =? "Blueman-manager")
       manageWindow
  ]
 where
  manageTerm = customFloating $ W.RationalRect l t w h
   where
    h = 0.9
    w = 0.9
    t = 0.95 - h
    l = 0.95 - w
  manageWindow = customFloating $ W.RationalRect l t w h
   where
    h = 0.6
    w = 0.6
    t = 0.85 - h
    l = 0.80 - w
