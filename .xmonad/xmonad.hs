import           Control.Concurrent         (forkIO, threadDelay)
import           Control.Monad              (liftM2, void)
import           Data.Default               (def)
import qualified Data.Map                   as M
import           XMonad
import           XMonad.Config.Xfce
import           XMonad.Hooks.ManageDocks
import           XMonad.Hooks.ManageHelpers
import           XMonad.Layout.Fullscreen
import           XMonad.Layout.Named (named)
import           XMonad.Layout.NoBorders
import           XMonad.Layout.PerWorkspace (onWorkspace)
import qualified XMonad.StackSet as W

myWorkspaces = ["1:dev", "2:web"] ++ map show [3..6] ++ ["7:spotify", "8:slack", "9:full"]

myManageHook = composeAll
    [ (className =? "Chromium") --> viewShift "2:web"
    , isDialog                  --> doCenterFloat
    , isFullscreen              --> (doF W.focusDown <+> doFullFloat)]
  where
    viewShift = doF . liftM2 (.) W.greedyView W.shift

basicLayout = Tall nmaster delta ratio
  where
    nmaster = 1
    delta   = 3/100
    ratio   = 1/2

tallLayout = named "tall" $ avoidStruts $ basicLayout

wideLayout = named "wide" $ avoidStruts $ Mirror basicLayout

singleLayout = named "single" $ avoidStruts $ noBorders Full

fullscreenLayout = named "fullscreen" $ noBorders Full

myLayoutHook = smartBorders $ fullscreen $ normal
  where
    normal      = tallLayout ||| wideLayout ||| singleLayout
    fullscreen  = onWorkspace "9:full" fullscreenLayout

-- xfce4-panel and xmonad interact badly, so xfce4-panel needs to be restarted a little after xmonad is ready
fixPanel :: IO ()
fixPanel = void $ forkIO $ do
  threadDelay 2000000
  spawn "xfce4-panel -r"

main :: IO ()
main = fixPanel >> xmonad myConfig

myConfig = xfceConfig
  { modMask    = mod4Mask
  , terminal   = "xfce4-terminal"
  , layoutHook = myLayoutHook
  , manageHook = manageDocks <+> myManageHook
  }
