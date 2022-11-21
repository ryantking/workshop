module XMonad.Custom.XPrompt
  ( myXPConfig
  , calcPrompt
  ) where

import           Control.Arrow                  ( first )
import           Data.Char
import qualified Data.Map                      as M
import           XMonad
import qualified XMonad.Actions.Search         as S
import qualified XMonad.Custom.Colors.Nord     as Theme
import           XMonad.Prompt
import           XMonad.Prompt.Input
import qualified XMonad.StackSet               as W
import           XMonad.Util.Run

myXPKeymap :: M.Map (KeyMask, KeySym) (XP ())
myXPKeymap =
  M.fromList
    $  map
         (first $ (,) controlMask)
         [ (xK_z          , killBefore)
         , (xK_k          , killAfter)
         , (xK_a          , startOfLine)
         , (xK_e          , endOfLine)
         , (xK_m          , deleteString Next)
         , (xK_b          , moveCursor Prev)
         , (xK_f          , moveCursor Next)
         , (xK_BackSpace  , killWord Prev)
         , (xK_y          , pasteString)
         , (xK_g          , quit)
         , (xK_bracketleft, quit)
         ]
    ++ map
         (first $ (,) mod4Mask)
         [ (xK_BackSpace, killWord Prev)
         , (xK_f        , moveWord Next)
         , (xK_b        , moveWord Prev)
         , (xK_d        , killWord Next)
         , (xK_n        , moveHistory W.focusUp')
         , (xK_p        , moveHistory W.focusDown')
         ]
    ++ map
         (first $ (,) 0)
         [ (xK_Return   , setSuccess True >> setDone True)
         , (xK_KP_Enter , setSuccess True >> setDone True)
         , (xK_BackSpace, deleteString Prev)
         , (xK_Delete   , deleteString Next)
         , (xK_Left     , moveCursor Prev)
         , (xK_Right    , moveCursor Next)
         , (xK_Home     , startOfLine)
         , (xK_End      , endOfLine)
         , (xK_Down     , moveHistory W.focusUp')
         , (xK_Up       , moveHistory W.focusDown')
         , (xK_Escape   , quit)
         ]

myXPConfig :: XPConfig
myXPConfig = def
  { font = "xft:JetBrainsMono NF:antialias=true:hinting=true:size=18"
  , bgColor             = Theme.color00
  , fgColor             = Theme.color05
  , bgHLight            = Theme.color03
  , fgHLight            = Theme.color06
  , borderColor         = Theme.color00
  , promptBorderWidth   = 1
  , promptKeymap        = myXPKeymap
  , position            = Top
  , height              = 42
  , historySize         = 256
  , historyFilter       = id
  , defaultText         = []
  , autoComplete        = Nothing
  , showCompletionOnTab = False
  , searchPredicate     = S.isPrefixOf
  , alwaysHighlight     = True
  , maxComplRows        = Nothing
  }

calcPrompt :: XPConfig -> String -> X ()
calcPrompt c ans = inputPrompt c (trim ans)
  ?+ \input -> liftIO (runProcessWithInput "qalc" [input] "") >>= calcPrompt c
  where trim = f . f where f = reverse . dropWhile isSpace
