module XMonad.Custom.ManageHook
  ( myManageHook
  ) where

import           Data.Monoid
import           XMonad
import           XMonad.ManageHook              ( className
                                                , doShift
                                                , title
                                                )

import           XMonad.Hooks.ManageDocks
import           XMonad.Hooks.ManageHelpers

import           XMonad.Util.NamedScratchpad

import           XMonad.Custom.Scratchpads
import           XMonad.Custom.Variables

myManageHook :: XMonad.Query (Data.Monoid.Endo WindowSet)
myManageHook =
  composeAll
      [ className =? "confirm" --> doFloat
      , className =? "file_progress" --> doFloat
      , className =? "dialog" --> doFloat
      , className =? "download" --> doFloat
      , className =? "error" --> doFloat
      , className =? "notification" --> doFloat
      , className =? "pinentry-gtk-2" --> doFloat
      , className =? "splash" --> doFloat
      , className =? "toolbar" --> doFloat
      , className =? "Pauvcontrol" --> doCenterFloat
      , title =? "Bluetooth" --> doFloat
      , className =? "zoom" --> doShift (myWorkspaces !! 5)
      , isFullscreen --> doFullFloat
      ]
    <+> namedScratchpadManageHook myScratchPads
    <+> manageDocks
