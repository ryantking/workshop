module XMonad.Custom.Mouse
  ( myMouseBindings
  ) where

import qualified Data.Map                      as M
import qualified XMonad
import           XMonad
import qualified XMonad.StackSet               as W

myMouseBindings (XConfig { XMonad.modMask = modm }) = M.fromList
  [ ( (modm, button1)
    , \w -> focus w >> mouseMoveWindow w >> windows W.shiftMaster
    )
  , ((modm, button2), \w -> focus w >> windows W.shiftMaster)
  , ( (modm, button3)
    , \w -> focus w >> mouseResizeWindow w >> windows W.shiftMaster
    )
  ]

