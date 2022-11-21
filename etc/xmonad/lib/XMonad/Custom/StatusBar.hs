module XMonad.Custom.StatusBar
  ( myStatusBar
  ) where

import           XMonad.Hooks.StatusBar
import           XMonad.Hooks.StatusBar.PP

import           XMonad.Util.ClickableWorkspaces

import qualified XMonad.Custom.Colors.Nord     as Theme
import           XMonad.Custom.Variables

myStatusBar = statusBarProp
  ("xmobar ~/.config/xmonad/xmobar-" ++ Theme.colorScheme ++ ".hs")
  (clickablePP myXmobarPP)

myXmobarPP = xmobarPP
  { ppCurrent         = xmobarColor Theme.color15 "" . wrap "[" "]"
  , ppVisible         = xmobarColor "#FF0000" ""
  , ppHidden          = xmobarColor Theme.color08 ""
  , ppHiddenNoWindows = xmobarColor Theme.color03 ""
  , ppTitle           = xmobarColor Theme.color07 "" . shorten 60
  , ppSep             = "<fc=" ++ Theme.color03 ++ "> | </fc>"
  , ppUrgent          = xmobarColor Theme.color11 "" . wrap "!" "!"
  , ppExtras          = [windowCount]
  , ppOrder           = \(ws : l : t : ex) ->
                          [ws]
                            ++ ex
                            ++ [xmobarColor Theme.color15 "" $ wrap "[" "]" l ++ " " ++ t]
  }