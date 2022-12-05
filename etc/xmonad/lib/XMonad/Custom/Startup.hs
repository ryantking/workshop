module XMonad.Custom.Startup
  ( myStartupHook
  ) where

import           XMonad

import           XMonad.Hooks.SetWMName

import           XMonad.Util.SpawnOnce

import qualified XMonad.Custom.Colors.Nord     as Theme

myStartupHook :: X ()
myStartupHook = do
  spawn "killall trayer-srg"
  spawnOnce "feh --bg-fill --no-fehbg ~/Pictures/wallpaper/blue-bond.jpg"
  spawnOnce "picom"
  spawn
    ("sleep 2 && trayer-srg --edge top --align right --SetDockType true --SetPartialStrut true --widthtype request --expand true --width 10 --transparent true --tint "
    ++ Theme.colorTrayer
    ++ " --alpha 0 --height 42"
    )
  setWMName "XMonad"
