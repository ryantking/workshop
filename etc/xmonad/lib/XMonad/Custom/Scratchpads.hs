module XMonad.Custom.Scratchpads
  ( myScratchPads
  ) where
import           XMonad
import           XMonad.Custom.Variables
import qualified XMonad.StackSet               as W
import           XMonad.Util.NamedScratchpad

myScratchPads :: [NamedScratchpad]
myScratchPads =
  [ NS "terminal"   spawnTerm findTerm manageTerm
  , NS "calculator" spawnCalc findCalc manageCalc
  ]
 where
  spawnTerm  = myTerminal ++ " -T tscratchpad"
  findTerm   = title =? "tscratchpad"
  manageTerm = customFloating $ W.RationalRect l t w h
   where
    h = 0.9
    w = 0.9
    t = 0.95 - h
    l = 0.95 - w
  spawnCalc  = "qalculate-gtk"
  findCalc   = className =? "Qalculate-gtk"
  manageCalc = customFloating $ W.RationalRect l t w h
   where
    h = 0.5
    w = 0.4
    t = 0.75 - h
    l = 0.70 - w
