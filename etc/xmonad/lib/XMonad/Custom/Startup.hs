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
  spawnOnce "feh --bg-fill --no-fehbg ~/Pictures/wallpaper/building.png"
  spawnOnce "xrandr -s 3840x2160 --dpi 144"
  spawnOnce "picom"
  spawnOnce "nextcloud"
  spawnOnce "flameshot"
  spawn
    ("sleep 2 && trayer-srg --edge top --align right --SetDockType true --SetPartialStrut true --widthtype request --expand true --width 10 --transparent true --tint "
    ++ Theme.colorTrayer
    ++ " --alpha 0 --height 30 --padding 2 --distancefrom top --distance 5"
    )
  setWMName "XMonad"
