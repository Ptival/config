import Data.Default
import qualified Data.Map as M
import XMonad
import XMonad.Actions.UpdatePointer
import XMonad.Actions.RotSlaves
import XMonad.Actions.TopicSpace
import XMonad.Config.Azerty
import XMonad.Hooks.ManageDocks
import XMonad.Layout.BoringWindows
import XMonad.Layout.Grid
import XMonad.Layout.NoBorders
import XMonad.Layout.PerWorkspace
import XMonad.Util.Paste
import XMonad.Util.Run

--myTerminal = "urxvt -depth 32 -fg white -bg black -sr -bc"
myTerminal = "xfce4-terminal"

myTopics = ["1", "2", "3", "4", "5", "6", "7", "8", "9"]

myTopicConfig = TopicConfig
    { defaultTopicAction    = const $ spawnTerminal >*> 3
    , defaultTopic          = "term"
    , maxTopicHistory       = 10
    , topicDirs             = M.fromList $
        []
    , topicActions          = M.fromList $
        []
    }

myKeys conf@(XConfig {modMask = modm}) = M.fromList $
    [ ((modm, xK_m), focusMaster)
    , ((modm, xK_j),   focusDown)
    , ((modm, xK_k),   focusUp)
--    , ((modm,               xK_Tab), rotAllDown)
--    , ((modm .|. shiftMask, xK_Tab), rotAllUp)
    ]

spawnTerminal   = spawn $ myTerminal

tall_1_2 = Tall 1 (1/100) (50/100)

myLayoutHook =
    avoidStruts $
    (noBorders Full ||| tall_1_2 ||| Mirror tall_1_2)

myLogHook = return () --updatePointer Nearest

myConfig = do
    checkTopicConfig myTopics myTopicConfig
    return $ def
        { focusedBorderColor    = "#ffffff"
        , handleEventHook       = docksEventHook
        , keys                  = \c -> myKeys c `M.union` keys def c
        , layoutHook            = myLayoutHook
        , logHook               = myLogHook
        , manageHook            = manageDocks
        , modMask               = mod4Mask
        , normalBorderColor     = "#0000ff"
        , terminal              = myTerminal
        , workspaces            = myTopics
        }

main = do
  xmproc <- spawnPipe "xmobar ~/config/xmobar.hs"
  xmonad =<< myConfig

