import qualified Data.Map                      as M
import           Data.Monoid
import           System.Exit
import           System.IO                      ( hPutStrLn )
import           XMonad

import qualified XMonad.StackSet               as W

import           XMonad.Hooks.EwmhDesktops
import           XMonad.Hooks.InsertPosition
import           XMonad.Hooks.ManageDocks
import           XMonad.Hooks.Minimize
import           XMonad.Hooks.ServerMode
import           XMonad.Hooks.StatusBar

import qualified XMonad.Custom.Colors.Nord     as Theme
import           XMonad.Custom.Keys
import           XMonad.Custom.Layouts
import           XMonad.Custom.ManageHook
import           XMonad.Custom.Mouse
import           XMonad.Custom.Startup
import           XMonad.Custom.StatusBar
import           XMonad.Custom.Variables
import           XMonad.Util.EZConfig

main = do
  myKeys <- myKeys
  xmonad
    .                 withSB myStatusBar
    .                 docks
    .                 ewmhFullscreen
    $                 ewmh def
                        { modMask            = myModMask
                        , terminal           = myTerminal
                        , startupHook        = myStartupHook
                        , workspaces         = myWorkspaces
                        , borderWidth        = myBorderWidth
                        , normalBorderColor  = myNormColor
                        , focusedBorderColor = myFocusColor
                        , mouseBindings      = myMouseBindings
                        , layoutHook         = myLayouts
                        , manageHook = insertPosition End Newer <+> myManageHook
                        , handleEventHook    = serverModeEventHookCmd
                                               <+> serverModeEventHook
                                               <+> serverModeEventHookF "XMONAD_PRINT"
                                                                        (io . putStrLn)
                                               <+> minimizeEventHook
                        }
    `additionalKeysP` myKeys
