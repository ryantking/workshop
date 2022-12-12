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
  { ppCurrent = xmobarColor Theme.color08 ""
  , ppVisible = xmobarColor "#FF0000" ""
  , ppHidden  = xmobarColor Theme.color03 ""
  , ppTitle   = xmobarColor Theme.color04 "" . shorten 60
  , ppSep     = xmobarColor Theme.color03 "" "  |  "
  , ppUrgent  = xmobarColor Theme.color11 "" . wrap "!" "!"
  , ppOrder   = \(ws : l : t : ex) ->
                  [ws]
                    ++ ex
                    ++ [ xmobarColor Theme.color09 "" l
                         ++ (xmobarColor Theme.color03 "" "  |  ")
                         ++ t
                       ]
  }
