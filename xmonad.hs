import qualified Data.Map as M
import XMonad
import XMonad.Actions.UpdatePointer
import XMonad.Actions.TopicSpace
import XMonad.Config.Azerty
import XMonad.Hooks.ManageDocks
import XMonad.Layout.Grid
import XMonad.Layout.NoBorders
import XMonad.Layout.PerWorkspace
import XMonad.Util.Paste

myBrowser   = "chromium"
myIM        = "pidgin"
--myTerminal  = "xterm -u8"
myTerminal  = "urxvt -depth 32 -fg white -bg black -sr -bc"

myTopics = ["term", "web", "irc", "im", "5", "6", "7", "8", "9"]

myTopicConfig = TopicConfig
    { defaultTopicAction    = const $ spawnTerminal >*> 3
    , defaultTopic          = "term"
    , maxTopicHistory       = 10
    , topicDirs             = M.fromList $
        []
    , topicActions          = M.fromList $
        [ ("web", spawnBrowser)
        , ("irc", spawnIRC)
        , ("im", spawnIM)
        ]
    }

myKeys conf@(XConfig {modMask = modm}) = M.fromList $
    [ ((modm,               xK_p), spawn "dmenu_run")
    ]

spawnBrowser    = spawn $ myBrowser
spawnIM         = spawn $ myIM
spawnIRC        = spawn $ "ssh -t pichon \"tmux attach -t irssi\""
spawnTerminal   = spawn $ myTerminal

tall_1_2 = Tall 1 (1/100) (50/100)
tall_2_3 = Tall 1 (1/100) (65/100)

myLayoutHook =
    avoidStruts $
    onWorkspaces ["irc", "web"] (noBorders  Full ||| tall_2_3) $
    (Grid ||| tall_1_2 ||| noBorders Full)

myLogHook = updatePointer (Relative 0.5 0.5)

myConfig = do
    checkTopicConfig myTopics myTopicConfig
    return $ defaultConfig
        { workspaces            = myTopics
        , terminal              = myTerminal
        , normalBorderColor     = "#0000ff"
        , focusedBorderColor    = "#ff0000"
        , modMask               = mod4Mask
        , layoutHook            = myLayoutHook
        , logHook               = myLogHook
        , manageHook            = manageDocks
        , keys                  =
            \c -> myKeys c `M.union`
                    azertyKeys c `M.union`
                    keys defaultConfig c
        }

main = xmonad =<< myConfig
