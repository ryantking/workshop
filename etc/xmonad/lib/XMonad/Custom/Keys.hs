module XMonad.Custom.Keys
  ( myKeys
  ) where

import qualified Data.Map                      as M
import           Data.Map
import           Graphics.X11.ExtraTypes
import           System.Exit
import           XMonad
import           XMonad.Actions.CopyWindow
import           XMonad.Actions.FloatKeys
import           XMonad.Actions.WithAll
import           XMonad.Layout.Gaps

import qualified XMonad.Layout.ToggleLayouts   as T
                                                ( ToggleLayout(Toggle)
                                                , toggleLayouts
                                                )
import qualified XMonad.StackSet               as W

import           XMonad.Actions.Minimize
import           XMonad.Actions.Navigation2D
import           XMonad.Actions.Promote
import           XMonad.Actions.RotSlaves
import           XMonad.Custom.Scratchpads
import           XMonad.Custom.Variables
import           XMonad.Custom.XPrompt
import           XMonad.Hooks.ManageDocks
import           XMonad.Layout.LimitWindows
import qualified XMonad.Layout.MultiToggle     as MT
import           XMonad.Layout.MultiToggle.Instances
import           XMonad.Layout.ResizableTile
import           XMonad.Layout.Spacing
import           XMonad.Layout.SubLayouts
import           XMonad.Prompt.Man
import           XMonad.Prompt.Shell
import           XMonad.Prompt.Ssh
import           XMonad.Prompt.Workspace
import           XMonad.Prompt.XMonad
import           XMonad.Util.NamedScratchpad

-- START_KEYS
myKeys :: IO [(String, X ())]
myKeys = do
  myEmacs <- myEmacs
  return
    -- KB_GROUP WM
    [ ("M-S-r", spawn "xmonad --recompile; xmonad --restart")
    , ("M-S-q"         , io exitSuccess)
    , ("M-S-/", spawn "/home/ryan/.config/xmonad/bin/show-keys.sh")
    , ("M-S-<Space>"   , spawn "dmenu_run")

      -- KB_GROUP Prompts
    , ("M-r <Space>"   , shellPrompt myXPConfig)
    , ("M-r x"         , xmonadPrompt myXPConfig)
    , ("M-r m"         , manPrompt myXPConfig)
    , ("M-r S-s"       , sshPrompt myXPConfig)
    , ("M-r c"         , calcPrompt myXPConfig "calc")
    , ("M-r S-o", workspacePrompt myXPConfig (windows . W.greedyView))

    -- KB_GROUP Kill
    , ("M-S-c"         , kill1)    -- Kill selected window
    , ("M-S-a"         , killAll)  -- Kill all windows in workspace

    -- KB_GROUP Float/Tile
    , ("M-f"           , sendMessage (T.Toggle "Floats")) -- Toggle floating layout
    , ("M-t"           , withFocused $ windows . W.sink)  -- Tile selected floating window
    , ("M-S-t"         , sinkAll)                         -- Tile all floating windows

    -- KB_GROUP Applications
    , ("M-<Return>"    , spawn myTerminal)
    , ("M-b p"         , spawn "pavucontrol-qt")
    , ("M-b M-b"       , spawn myBrowser)

    -- KB_GROUP Emacs
    , ("M-w w"         , spawn myEmacs)

    -- KB_GROUP Scratchpads
    , ("M-s t", namedScratchpadAction myScratchPads "terminal")
    , ("M-s c", namedScratchpadAction myScratchPads "calculator")

    -- KB_GROUP Floating Windows
    , ("M-M1-<Left>", withFocused (keysResizeWindow (20, 0) (1, 1)))
    , ("M-M1-<Right>", withFocused (keysResizeWindow (-20, 0) (1, 1)))
    , ("M-M1-<Up>", withFocused (keysResizeWindow (0, 20) (1, 1)))
    , ("M-M1-<Down>", withFocused (keysResizeWindow (0, -20) (1, 1)))
    , ("M-M1-S-<Left>", withFocused (keysMoveWindow (-20, 0)))
    , ("M-M1-S-<Right>", withFocused (keysMoveWindow (20, 0)))
    , ("M-M1-S-<Up>", withFocused (keysMoveWindow (0, -20)))
    , ("M-M1-S-<Down>", withFocused (keysMoveWindow (0, 20)))

    -- KB_GROUP Clients
    , ("M-h"           , windows W.focusMaster)          -- Move focus to the master window
    , ("M-M1-C-n"      , windowGo D False)               -- Move focus down
    , ("M-M1-C-e"      , windowGo U False)               -- Move focus up
    , ("M-m"           , windowGo L False)               -- Move focus left
    , ("M-i"           , windowGo R False)               -- Move focus right
    , ("M-S-h"         , windows W.swapMaster)           -- Swap the focused window and the master window
    , ("M-M1-C-n"      , windowSwap D False)             -- Swap focused window down
    , ("M-S-e"         , windowSwap U False)             -- Swap focused window up
    , ("M-S-m"         , windowSwap L False)             -- Swap focused window left
    , ("M-S-i"         , windowSwap R False)             -- Swap focused window right
    , ("M-e"           , windows W.focusUp)              -- Focus up by id
    , ("M-n"           , windows W.focusDown)            -- Focus down by id
    , ("M-S-e"         , windows W.swapUp)               -- Swap up by id
    , ("M-S-n"         , windows W.swapDown)             -- Swap d own by id
    , ("M-<Backspace>" , promote)                        -- Moves focused window to master, others maintain order
    , ("M-M1-<Tab>"    , rotSlavesDown)                  -- Rotate all windows except master and keep focus in place
    , ("M-C-<Tab>"     , rotAllDown)                     -- Rotate all the windows in the current stack
    , ("M-S-<Up>"      , sendMessage (IncMasterN 1))     -- Increase # of clients master pane
    , ("M-S-<Down>"    , sendMessage (IncMasterN (-1)))  -- Decrease # of clients master pane
    , ("M-C-<Up>"      , increaseLimit)                  -- Increase # of windows
    , ("M-C-<Down>"    , decreaseLimit)                  -- Decrease # of windows
    , ("M-M1-m"        , sendMessage Shrink)             -- Shrink horiz window width
    , ("M-M1-i"        , sendMessage Expand)             -- Expand horiz window width
    , ("M-M1-n"        , sendMessage MirrorShrink)       -- Shrink vert window width
    , ("M-M1-e"        , sendMessage MirrorExpand)       -- Expand vert window width
    , ("M-C-m"         , sendMessage $ pullGroup L)
    , ("M-C-i"         , sendMessage $ pullGroup R)
    , ("M-C-e"         , sendMessage $ pullGroup U)
    , ("M-C-n"         , sendMessage $ pullGroup D)
    , ("M-C-h", withFocused (sendMessage . MergeAll))
    , ("M-C-u"         , withFocused (sendMessage . UnMerge))
    , ("M-C-/", withFocused (sendMessage . UnMergeAll))
    , ("M-C-."         , onGroup W.focusUp')             -- Switch focus to next tab
    , ("M-C-,"         , onGroup W.focusDown')           -- Switch focus to prev tab
    , ("M-k"           , withFocused minimizeWindow)
    , ("M-S-n", withLastMinimized maximizeWindowAndFocus)

    -- KB_GROUP Window Spacing
    , ("M-C-S-e"       , decWindowSpacing 4)         -- Decrease window spacing
    , ("M-C-S-n"       , incWindowSpacing 4)         -- Increase window spacing

    -- KB_GROUP Layouts
    , ("M1-<Tab>"      , sendMessage NextLayout)                                     -- Go to next layout
    , ("M1-S-<Tab>"    , sendMessage FirstLayout)                                     -- Go to next layout
    , ("M-<Space>", sendMessage (MT.Toggle NBFULL) >> sendMessage ToggleStruts) -- Toggle struts
    , ("M-C-b"         , sendMessage (MT.Toggle NBFULL))
    , ("M-S-b"         , sendMessage ToggleStruts)

    -- KB_GROUP Media
    , ("M-u <Space>"   , spawn "playerctl pause")
    , ("M-u S-<Space>" , spawn "playerctl play")
    , ("M-u p"         , spawn "playerctl previous")
    , ("M-u n"         , spawn "playerctl next")
    ]
    -- END_KEYS
