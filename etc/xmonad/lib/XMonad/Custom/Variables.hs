module XMonad.Custom.Variables where

import           System.Posix.User
import           XMonad
import           XMonad.StackSet               as W

import qualified XMonad.Custom.Colors.Nord     as Theme


myFont :: String
myFont = "xft:Iosevka Custom:regular:size=16:antialias=true:hinting=true"

myModMask :: KeyMask
myModMask = mod4Mask

myTerminal :: String
myTerminal = "st"

myBrowser :: String
myBrowser = "librewolf"

myEmacs :: IO String
myEmacs = do
  uid <- getRealUserID
  return
    (  "emacsclient --socket-name /tmp/emacs"
    ++ show uid
    ++ "/server -c -a 'emacs' "
    )

myDmenu :: String
myDmenu = "dmenu_run"

myBorderWidth :: Dimension
myBorderWidth = 6

myNormColor :: String
myNormColor = Theme.color00

myFocusColor :: String
myFocusColor = Theme.color15

windowCount :: X (Maybe String)
windowCount =
  gets
    $ Just
    . show
    . length
    . W.integrate'
    . W.stack
    . W.workspace
    . W.current
    . windowset

myWorkspaces :: [String]
myWorkspaces =
  ["dev", "www", "work", "chat", "audio", "video", "sys", "game", "misc"]

