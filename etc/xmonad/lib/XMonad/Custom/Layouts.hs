module XMonad.Custom.Layouts
  ( myLayouts
  ) where

import           XMonad

import           XMonad.Actions.MouseResize

import           XMonad.Hooks.ManageDocks

import           XMonad.Layout.GridVariants     ( Grid(Grid) )
import           XMonad.Layout.LayoutModifier
import           XMonad.Layout.LimitWindows
import           XMonad.Layout.Magnifier
import           XMonad.Layout.Minimize
import           XMonad.Layout.MultiToggle
import           XMonad.Layout.MultiToggle.Instances
import           XMonad.Layout.NoBorders
import           XMonad.Layout.PerWorkspace     ( onWorkspace )
import           XMonad.Layout.Reflect
import           XMonad.Layout.Renamed
import           XMonad.Layout.ResizableTile
import           XMonad.Layout.Simplest
import           XMonad.Layout.SimplestFloat
import           XMonad.Layout.Spacing
import           XMonad.Layout.SubLayouts
import           XMonad.Layout.Tabbed
import           XMonad.Layout.ThreeColumns
import qualified XMonad.Layout.ToggleLayouts   as T
                                                ( ToggleLayout(Toggle)
                                                , toggleLayouts
                                                )
import           XMonad.Layout.WindowArranger
import           XMonad.Layout.WindowNavigation

import qualified XMonad.Custom.Colors.Nord     as Theme
import           XMonad.Custom.Variables


mySpacing
  :: Integer -> l a -> XMonad.Layout.LayoutModifier.ModifiedLayout Spacing l a
mySpacing i = spacingRaw False (Border i i i i) True (Border i i i i) True

mySpacing'
  :: Integer -> l a -> XMonad.Layout.LayoutModifier.ModifiedLayout Spacing l a
mySpacing' i = spacingRaw True (Border i i i i) True (Border i i i i) True

tall =
  renamed [Replace "Tall"]
    $ smartBorders
    $ windowNavigation
    $ addTabs shrinkText myTabTheme
    $ subLayout [] (smartBorders Simplest)
    $ limitWindows 12
    $ mySpacing' 10
    $ minimize
    $ ResizableTall 1 (3 / 100) (1 / 2) []
tallR =
  renamed [Replace "TallR"]
    $ smartBorders
    $ windowNavigation
    $ addTabs shrinkText myTabTheme
    $ subLayout [] (smartBorders Simplest)
    $ limitWindows 12
    $ mySpacing' 10
    $ reflectHoriz
    $ minimize
    $ ResizableTall 1 (3 / 100) (1 / 2) []
magnify =
  renamed [Replace "Magnify"]
    $ smartBorders
    $ windowNavigation
    $ addTabs shrinkText myTabTheme
    $ subLayout [] (smartBorders Simplest)
    $ magnifier
    $ limitWindows 12
    $ mySpacing 16
    $ minimize
    $ ResizableTall 1 (3 / 100) (1 / 2) []
monocle =
  renamed [Replace "Monocle"]
    $ smartBorders
    $ windowNavigation
    $ addTabs shrinkText myTabTheme
    $ subLayout [] (smartBorders Simplest)
    $ minimize
    $ limitWindows 20 Full
floats = renamed [Replace "Floats"] $ smartBorders $ minimize $ limitWindows
  20
  simplestFloat
grid =
  renamed [Replace "Grid"]
    $ smartBorders
    $ windowNavigation
    $ addTabs shrinkText myTabTheme
    $ subLayout [] (smartBorders Simplest)
    $ limitWindows 12
    $ mySpacing' 16
    $ mkToggle (single MIRROR)
    $ minimize
    $ Grid (16 / 10)
threeCol =
  renamed [Replace "ThreeCol"]
    $ smartBorders
    $ windowNavigation
    $ addTabs shrinkText myTabTheme
    $ subLayout [] (smartBorders Simplest)
    $ limitWindows 15
    $ mySpacing' 4
    $ minimize
    $ ThreeCol 1 (3 / 100) (5 / 12)
tabs = renamed [Replace "Tabs"] $ minimize $ tabbed shrinkText myTabTheme

myTabTheme = def { fontName            = myFont
                 , activeColor         = Theme.color00
                 , inactiveColor       = Theme.color00
                 , activeBorderColor   = Theme.color00
                 , inactiveBorderColor = Theme.color00
                 , activeTextColor     = Theme.color15
                 , inactiveTextColor   = Theme.color03
                 }

myLayoutHook =
  avoidStruts $ mouseResize $ windowArrange $ T.toggleLayouts floats $ mkToggle
    (NBFULL ?? NOBORDERS ?? EOT)
    myDefaultLayout
 where
  myDefaultLayout =
    withBorder myBorderWidth tall
      ||| withBorder myBorderWidth tallR
      ||| noBorders monocle
      ||| XMonad.Custom.Layouts.magnify
      ||| noBorders tabs
      ||| withBorder myBorderWidth grid
      ||| withBorder myBorderWidth threeCol

myLayoutHook1 =
  avoidStruts $ mouseResize $ windowArrange $ T.toggleLayouts floats $ mkToggle
    (NBFULL ?? NOBORDERS ?? EOT)
    myDefaultLayout
 where
  myDefaultLayout =
    withBorder myBorderWidth tall ||| noBorders monocle ||| noBorders tabs

myLayouts = onWorkspace (myWorkspaces !! 0) myLayoutHook1 $ myLayoutHook
