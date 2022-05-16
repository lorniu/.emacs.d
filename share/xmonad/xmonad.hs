{- szarch/svarch

   Sample: /usr/share/xmonad/man/xmonad.hs
   Document: https://hackage.haskell.org/package/xmonad-contrib-0.16/docs/XMonad-Doc-Extending.html
   Reference: https://hackage.haskell.org/package/xmonad-contrib-0.16/docs/XMonad-Config-Prime.html
   Learning: https://github.com/fragilelambda/txmonad

* Download:

   cd $HOME/.config/xmonad/
   git clone https://github.com/xmonad/xmonad
   git clone https://github.com/xmonad/xmonad-contrib
   git clone https://github.com/jaor/xmobar

   echo "packages: */*.cabal" > cabal.project

* Installation:

   cabal install xmonad xmonad-contrib --lib [--package-env=$HOME/.config/xmonad]
   cabal install xmonad
   cabal install xmobar -f with_xft -f with_xpm -f with_inotify

   vi ~/.ghc/*/environments/defaults [or $HOME/.config/xmonad/*/environments..]

* Clean:

   GHC=ghc-8.10.7
   rm ~/.cabal/bin/xmobar
   rm ~/.cabal/bin/xmonad
   rm -rf ~/.cabal/logs/$GHC/{xmobar,xmonad}*
   rm -rf ~/.cabal/store/$GHC/{xmobar,xmonad}*
   rm -rf ~/.cabal/packages/*/{xmobar,xmonad}*

* Applications:

   dmenu                  : yes, menu
   jgmenu                 : yes, app menu
   xdotool                : click event
   xcompmgr               : shadow effects and more
   slock                  : screen saver | xautolock -time 15 -locker slock
   feh                    : image viewer
   dunst                  : notification
   brightnessctl          : xbacklight
   alacritty              : terminal
   nemo                   : file manager
   qutebrowser            : browser
   deepin-screen-recorder : screenshot, to fix Paste issue, install with 'yay -S deepin-screenshot-copy-patch'

   removed: gnome-screenshot/gscreenshot/scrot/pcmanfm/xfce4-terminal/acpilight/tilix
-}

module Main where

import Control.Arrow (first)
import Data.Char
import Data.List
import Data.Maybe
import Data.Monoid
import Data.Time
import System.Exit
import System.IO
import System.Posix.Unistd
import System.Environment
import qualified Data.Map as M
import System.Directory
import XMonad
import qualified XMonad.StackSet as W
import XMonad.Actions.CopyWindow
import XMonad.Actions.CycleWS (nextScreen, prevScreen, shiftNextScreen)
import XMonad.Actions.WithAll (withAll, sinkAll)
import XMonad.Actions.SpawnOn (manageSpawn)
import XMonad.Actions.SwapWorkspaces (swapTo)
import XMonad.Actions.MouseResize (mouseResize)
import XMonad.Actions.Minimize (withLastMinimized, minimizeWindow, maximizeWindowAndFocus)
import XMonad.Actions.FloatKeys (keysResizeWindow, keysAbsResizeWindow, keysMoveWindow, keysMoveWindowTo)
import qualified XMonad.Actions.FlexibleManipulate as Flex
import XMonad.Actions.Commands
import XMonad.Hooks.SetWMName (setWMName)
import XMonad.Hooks.Place (placeHook, withGaps, simpleSmart)
import XMonad.Hooks.ManageHelpers (isDialog, composeOne, transience, (-?>), doRectFloat, isFullscreen, doFullFloat)
import XMonad.Hooks.ManageDocks (avoidStruts, docks, manageDocks, ToggleStruts(..))
import XMonad.Hooks.PositionStoreHooks (positionStoreManageHook, positionStoreEventHook)
import XMonad.Hooks.StatusBar
import XMonad.Hooks.StatusBar.PP
import XMonad.Prompt
import XMonad.Prompt.FuzzyMatch (fuzzyMatch)
import XMonad.Prompt.Man (manPrompt)
import XMonad.Prompt.Shell (shellPrompt)
import XMonad.Prompt.XMonad (xmonadPromptC)
import XMonad.Util.Run (spawnPipe)
import XMonad.Util.NamedScratchpad (NamedScratchpad(NS), customFloating, defaultFloating, namedScratchpadManageHook, namedScratchpadAction)
import XMonad.Util.Cursor (setDefaultCursor)
import XMonad.Util.SpawnOnce (spawnOnce)
import XMonad.Util.EZConfig (additionalKeys, additionalKeysP, additionalMouseBindings)
import XMonad.Layout.Named (named)
import XMonad.Layout.Spacing (spacingRaw, incWindowSpacing, decWindowSpacing, Border(..))
import XMonad.Layout.Minimize (minimize)
import XMonad.Layout.Maximize (maximizeWithPadding, maximizeRestore)
import XMonad.Layout.NoBorders
import XMonad.Layout.PerWorkspace (onWorkspace)
import XMonad.Layout.WindowArranger (windowArrange)          -- move/resize windows with keyboard
import XMonad.Layout.PositionStoreFloat (positionStoreFloat) -- floating layout with dual-head setup
import XMonad.Layout.MagicFocus (followOnlyIf, disableFollowOnWS) -- disable floated WS's follow
import XMonad.Layout.BoringWindows (boringWindows, focusDown, focusUp, focusMaster) -- when focus change, ignore minimized windows
import XMonad.Layout.IndependentScreens (countScreens)
import XMonad.Layout.NoFrillsDecoration
import XMonad.Layout.MultiToggle (mkToggle, single, EOT(EOT), (??), Toggle(..))
import XMonad.Layout.MultiToggle.Instances (StdTransformers(NBFULL, MIRROR, NOBORDERS))
import XMonad.Layout.TrackFloating (trackFloating)  -- at last, I found it!
import XMonad.Layout.Grid
import XMonad.Layout.Circle
import XMonad.Layout.OneBig
import XMonad.Layout.ThreeColumns
import XMonad.Layout.MouseResizableTile
import XMonad.Layout.BinarySpacePartition


{- Entrance -}

main = do
  debugLog
  xmonad $ dynamicSBs myxBar . docks $ configs

debugLog = do
  trace "\n\n[xmonad] starting ..."
  getZonedTime >>= \time -> trace $ "[xmonad] " ++ show time
  myWallpaper >>= \w -> trace $ "[xmonad] Wallpaper: " ++ w ++ "\n\n"


{- Customer -}

configs = def
  { modMask            = mod4Mask
  , terminal           = myTerminal
  , workspaces         = wss

  , borderWidth        = 1
  , normalBorderColor  = "#555555"
  , focusedBorderColor = "skyblue"
  , focusFollowsMouse  = True
  , clickJustFocuses   = True

  , startupHook        = myStartup  -- called after started
  , layoutHook         = myLayout   -- called when workspace loaded
  , manageHook         = myManage   -- called when a new window is created.
  , logHook            = myLogHook  -- called when the stack of windows managed has been changed.
  , handleEventHook    = myEvents   -- called on all events handled by xmonad.
  }
  `additionalKeys` myBindings2 `additionalKeys` myBindings1 `additionalKeys` myBindings4 `additionalKeysP` myBindings3 `additionalMouseBindings` myMouseBindings

configdir        = "~/.notes/share/xmonad/"
wss              = ["1.W", "2.M", "3.T", "4.S", "f", "6", "IMG", "8", "NSP"]

myTerminal       = "alacritty"
myTerminalCfg    = "gsettings set org.cinnamon.desktop.default-applications.terminal exec " ++ myTerminal
myTerminalCfgArg = "gsettings set org.cinnamon.desktop.default-applications.terminal exec-arg '--title=floater'"

screenShotArea   = "deepin-screen-recorder"
screenShotFull   = "deepin-screen-recorder -f"
screenSaver      = "slock"

textEditor       = "emacsclient -a 'emacs' "
imageEditor      = findExecutable "krita" >>= \krita -> return $ if isNothing krita then "gimp" else "krita"

myWallpaper      = do
  wp <- lookupEnv "XMONAD_WALLPAPER"
  case wp of Just w   -> return w
             Nothing  -> myFindM doesFileExist (configdir ++ "wallpaper.png") imgs :: IO String
  where imgs = ["/home/vip/images/wallpaper." ++ c | c <- ["png", "jpg", "jpeg", "bmp"]]


{- Keybindings -}

myBindings1 = [ (( m                 , xK_h      ) , sendMessage Shrink )
              , (( m                 , xK_l      ) , sendMessage Expand )
              , (( m                 , xK_o      ) , nextScreen  )
              , (( m .|. shiftMask   , xK_o      ) , shiftNextScreen >> nextScreen )
              , (( m                 , xK_Tab    ) , focusDown   )
              , (( m .|. shiftMask   , xK_Tab    ) , focusUp     )
              , (( m                 , xK_j      ) , focusDown   )
              , (( m                 , xK_k      ) , focusUp     )
              , (( m                 , xK_m      ) , focusMaster )
              , (( m .|. shiftMask   , xK_m      ) , windows W.swapMaster )
              , (( m .|. shiftMask   , xK_k      ) , windows W.swapUp     )
              , (( m .|. shiftMask   , xK_j      ) , windows W.swapDown   )
              , (( m                 , xK_comma  ) , sendMessage (IncMasterN 1) )
              , (( m                 , xK_period ) , sendMessage (IncMasterN (-1)) )
              , (( m .|. controlMask , xK_Left   ) , swapTo Prev )
              , (( m .|. controlMask , xK_Right  ) , swapTo Next )
              , (( m .|. shiftMask   , xK_c      ) , kill1 )

              , (( m                 , xK_b      ) , sendMessage $ ToggleStruts )
              , (( m                 , xK_space  ) , sendMessage $ Toggle NBFULL )
              , (( m .|. shiftMask   , xK_space  ) , sendMessage $ Toggle MIRROR )
              , (( m .|. shiftMask   , xK_n      ) , sendMessage $ NextLayout )
              , (( m                 , xK_Return ) , withFocused $ sendMessage . maximizeRestore )

              , (( m                 , xK_t      ) , withFocused floatCurrent                   )
              , (( m .|. shiftMask   , xK_t      ) , withFocused floatAll                       )
              , (( m                 , xK_d      ) , withFocused minimizeWindow                 )
              , (( m                 , xK_f      ) , withLastMinimized maximizeWindowAndFocus   )

              , (( m                 , xK_q      ) , scratchpadOpen "icalingua" )
              , (( m                 , xK_w      ) , scratchpadOpen "browser" )
              , (( m                 , xK_e      ) , scratchpadOpen "dired" )
              , (( m .|. shiftMask   , xK_e      ) , spawn "nemo --no-desktop" )
              , (( m                 , xK_r      ) , scratchpadOpen "terminal" )
              , (( m .|. shiftMask   , xK_r      ) , spawn (myTerminal ++ " --title=floater") >> windows W.shiftMaster)
              , (( 0                 , xK_Print  ) , spawn screenShotArea )
              , (( m                 , xK_Print  ) , spawn screenShotFull )
              , (( m                 , xK_p      ) , spawn "dmenu_run" )
              , (( m .|. shiftMask   , xK_p      ) , shellPrompt myXPConfig )
              , (( m                 , xK_g      ) , xmonadPromptC commands myXPConfig )
              , (( m                 , xK_F5     ) , spawn "xmonad --recompile ; killall xmobar ; xmonad --restart" )
              ] where m = mod4Mask

myBindings2 = [ (( m                 , xK_u      ) , withFocusedFloat $ moveAndResize (1050, 620) (850, 560))
              , (( m                 , xK_o      ) , withFocusedFloat $ keysResizeWindow (10, 10) (1, 1))
              , (( m                 , xK_i      ) , withFocusedFloat $ keysResizeWindow (-10, -10) (1, 1))
              , (( m .|. shiftMask   , xK_i      ) , withFocusedFloat $ keysResizeWindow (-10, -10) (1/2, 1/2))
              , (( m .|. shiftMask   , xK_o      ) , withFocusedFloat $ keysResizeWindow (10, 10) (1/2, 1/2))
              , (( m                 , xK_h      ) , withFocusedFloat $ keysMoveWindow (-10, 0)) -- left
              , (( m                 , xK_j      ) , withFocusedFloat $ keysMoveWindow (0, 10))  -- down
              , (( m                 , xK_k      ) , withFocusedFloat $ keysMoveWindow (0, -10)) -- up
              , (( m                 , xK_l      ) , withFocusedFloat $ keysMoveWindow (10, 0))  -- right
              , (( m .|. controlMask , xK_h      ) , withFocusedFloat $ keysMoveWindow (-1, 0))
              , (( m .|. controlMask , xK_j      ) , withFocusedFloat $ keysMoveWindow (0, 1))
              , (( m .|. controlMask , xK_k      ) , withFocusedFloat $ keysMoveWindow (0, -1))
              , (( m .|. controlMask , xK_l      ) , withFocusedFloat $ keysMoveWindow (1, 0))
              , (( m .|. shiftMask   , xK_h      ) , withFocusedFloat $ keysResizeWindow (20, 0) (1, 0))
              , (( m .|. shiftMask   , xK_j      ) , withFocusedFloat $ keysResizeWindow (0, -20) (0, 1))
              , (( m .|. shiftMask   , xK_k      ) , withFocusedFloat $ keysResizeWindow (0, 20) (0, 1))
              , (( m .|. shiftMask   , xK_l      ) , withFocusedFloat $ keysResizeWindow (-20, 0) (1, 0))
              , (( m                 , xK_m      ) , withFocusedFloat $ keysMoveWindowTo (960, 600) (1/2, 1/2))
              , (( m                 , xK_b      ) , withFocusedFloat $ keysMoveWindowTo (960, 1180) (1/2, 1))
              , (( m                 , xK_n      ) , withFocusedFloat $ keysMoveWindowTo (20, 1180) (0, 1))
              , (( m                 , xK_comma  ) , withFocusedFloat $ keysMoveWindowTo (1900, 1180) (1, 1))
              ] where m = mod3Mask

myBindings3 = [ ( "<XF86AudioMute>"          , spawn "isend sound toggle" )
              , ( "<XF86AudioLowerVolume>"   , spawn "isend sound down" )
              , ( "<XF86AudioRaiseVolume>"   , spawn "isend sound up" )
              , ( "<XF86MonBrightnessDown>"  , spawn "brightnessctl s 5-" )
              , ( "<XF86MonBrightnessUp>"    , spawn "brightnessctl s 5+" )
              , ( "<XF86AudioPlay>"          , spawn "mpc play" )
              , ( "<XF86AudioPause>"         , spawn "mpc pause" )
              , ( "<XF86AudioStop>"          , spawn "mpc stop" )
              , ( "<XF86AudioPrev>"          , spawn "mpc prev" )
              , ( "<XF86AudioNext>"          , spawn "mpc next" )
              , ( "<XF86ScreenSaver>"        , spawn screenSaver )
              , ( "M-c l"                    , spawn screenSaver )
              , ( "M-c t"                    , scratchpadOpen "htop" )
              , ( "M-c g"                    , scratchpadOpen "ghci" )
              , ( "M-c v"                    , scratchpadOpen "v2ray" )
              , ( "M-c p"                    , scratchpadOpen "powertop" )
              , ( "M1-C-a"                   , spawn screenShotArea )
              , ( "M-c M-p"                  , spawn screenShotFull )
              , ( "M-c e"                    , spawn "emacsclient --eval '(with-scratchpad-name \"floater\" (find-file \"~/vvv/dot.org\"))'" )
              ]

myBindings4 = [ ((m1 .|. n , k) , windows $ f w) -- M-1..9
                  | (n, f)   <- [ (0 , myGreedyView) , (shiftMask , W.shift) , (shiftMask .|. controlMask , copy) ]
                  , (k, w)   <- zip [xK_1 .. xK_9] wss
              ] ++
              [ ((m2 .|. n , k) , screenWorkspace i >>= flip whenJust (windows . f)) -- M-w..r
                  | (n, f)   <- [ (0 , W.view) , (shiftMask , W.shift) ]
                  , (k, i)   <- zip [xK_w, xK_e, xK_r] [0..]
              ] where m1 = mod4Mask; m2 = mod3Mask

myMouseBindings = [ ((m, button1) , mouseMoveFloat)
                  , ((m, button3) , mouseResizeFloat)
                  ] where m = mod4Mask


{- Common Hooks -}

-- Startup Action
myStartup :: X ()
myStartup = do setDefaultCursor xC_left_ptr  -- cursor style
               setWMName "LG3D"              -- LG3D, Looking Glass 3D: cheat to make old JDK work [opt]
               spawnOnce "xcompmgr -n"       -- -cCF -t-2 -l2 -r5 -o.3"
               spawnOnce "xmodmap -e 'add mod3 = Muhenkan'"
               spawnOnce "dunst -conf ~/.emacs.d/share/xmonad/dunst.conf"
               spawnOnce "blueman-applet"
               spawnOnce "fcitx5"
               spawnOnce =<< io (fmap ("feh --bg-scale " ++) myWallpaper)
               spawnOnce =<< io imageEditor
               spawnOnce myTerminalCfg
               spawnOnce myTerminalCfgArg

-- Execute arbitrary actions and WindowSet manipulations when managing a new window.
-- use 'xprop' to view WM info, and match a property use title or className
myManage :: ManageHook
myManage = manageDocks                               -- reveal Docks
           <+> manageSpawn                           -- spawnOn/spawnHere
           <+> positionStoreManageHook Nothing       -- PositionStore
           <+> namedScratchpadManageHook scratchpads -- named scratchpads support
           <+> composeAll [ isDialog                            --> doFloat
                          , className =? "mplayer"              --> doFloat
                          , className =? "lcalingua"            --> doFloat
                          , className =? "feh"                  --> doFloat
                          , className =^ "imdd."                --> doFloat
                          , className =^ "fcitx5-config"        --> doFloat
                          , className =^ "nm-connection"        --> doFloat
                          , className =^ "blueman-manager"      --> doFloat
                          , className =^ "pavucontrol"          --> doFloat
                          , className =^ "qutebrowser"          --> doFloat
                          , className =^ "VirtualBox Manager"   --> doFloat
                          , title     =? "Emulator"             --> doFloat
                          , title     =? "Android Emulator"     --> doFloat
                          , wmIcon    =? "floater"              --> doFloatToCenter
                          , wmRole    =? "Preferences"          --> doFloat
                          , className =? "Gimp"                 --> doShift (wss!!6)
                          , className =? "krita"                --> doShift (wss!!6)
                          , title     =? "floater"              --> doFloatRectPos (0.55 - 20/1920) (0.05 - 20/1200) 0.45 0.45
                          , title     =^ "aaa"                  --> doFloatToCenter
                          , title     =^ "app"                  --> doFloatToCenter
                          , title     =^ "ImageMagick"          --> doFloatToCenter
                          , title     =^ "test"                 --> doFloatToCenter
                          , resource  =? "desktop_window"       --> doIgnore
                          , isFullscreen                        --> doF W.focusDown <+> doFullFloat
                          ]
           <+> composeOne [ transience
                          , resource  =? "desktop_window"       -?> doIgnore
                          ]
  where l s = [ toLower c | c <- s ]; q =^ x = fmap (\c -> l x `isPrefixOf` l c) q

-- Layouts
myLayout = lessBorders Screen $ decorate
           $ onWorkspace (wss!!4) ( decorateF positionStoreFloat ) -- sane floating workspace
           $ mkToggle ( NBFULL ?? NOBORDERS ?? EOT )             -- [m-space] toggle full
           $ mkToggle ( single MIRROR )                          -- [m-s-space] toggle mirror
           $ normal ||| oneBig ||| triCol ||| bsp ||| grid       -- [m-s-n] switch layout
  where normal    = named "Normal" $ mouseResizableTile { draggerType = FixedDragger 1 5 }
        oneBig    = named "OneBig" $ OneBig (3/4) (3/4)
        triCol    = named "TriCol" $ ThreeColMid 1 (3/100) (1/2)
        bsp       = named "EmpBSP" $ emptyBSP
        grid      = named "-Grid-" $ Grid
        full      = named "FullSc" $ noBorders Full
        decorate  = avoidStruts                             -- adjust layout, don't cover up any dock-app
                    . boringWindows                         -- focus up/down/master
                    . windowArrange                         -- move or resize window with keyboard
                    . trackFloating                         -- don't switch fullwindow when focus float!
                    . minimize                              -- [m-d/f] hide window
                    . maximizeWithPadding 30                -- [m-enter] large overview
        decorateF = noBorders . mouseResize
                    . noFrillsDeco shrinkText noFrillTheme  -- enable title
                    . maximizeWithPadding 0

-- XMonad calls the logHook with every internal state update.
myLogHook = return ()

-- Defines a custom handler function for X Events.
myEvents :: Event -> X All
myEvents = positionStoreEventHook                           -- PositionStore
           <+> followOnlyIf (disableFollowOnWS [wss!!4])    -- disable autoFollow in this workspace


{- Commands -}

-- Prompt Commands
commands = [ ( "Manual Prompt"                   , manPrompt myXPConfig )
           , ( "Minor Layout (M-S-space)"        , sendMessage $ Toggle MIRROR )
           , ( "Decrease Window Spacing"         , decWindowSpacing 4 )
           , ( "Increase Window Spacing"         , incWindowSpacing 4 )
           , ( "*Eshell*"                        , spawn $ textEditor ++ " -c -e '(eshell)'" )
           ]

-- Named Toggle-Floating App
scratchpads = [ NS "terminal"
                    ( termCmd "terminal" "" )
                    ( title =? "scratch-terminal" )
                    ( rectFloating (0.55 - 20/1920) (0.55 - 20/1200) 0.45 0.45 ),
                NS "browser"
                    ( emacsCmd "browser" "(eaf-open-browser-with-history)" )
                    ( wmIcon =? "scratch-browser" )
                    ( rectFloating 0.25 0.2 0.5 0.6 ),
                NS "dired"
                    ( emacsCmd "dired" "(dired nil)" )
                    ( wmIcon =? "scratch-dired" )
                    ( rectFloating 0.18 0.15 0.65 0.7 ),
                NS "v2ray"
                    ( termCmd "v2ray" "journalctl -u v2ray -f" )
                    ( title =? "scratch-v2ray" )
                    ( rectFloating 0.42 0.06 0.57 0.35 ),
                NS "powertop"
                    ( termCmd "powertop" "sudo powertop" )
                    ( title =? "scratch-powertop" )
                    ( defaultFloating ),
                NS "icalingua"
                    ( "icalingua" )
                    ( className =? "icalingua" )
                    ( rectFloating 0.45 0.3 0.5 0.6 )
              ] ++
              map terminalApp [ "ghci", "htop" ]
  where
    rectFloating x y a b = customFloating $ W.RationalRect x y a b
    termCmd title cmd    = myTerminal ++ " --title=scratch-" ++ title ++ (if cmd == "" then "" else " -e " ++ cmd)
    emacsCmd title cmd   = "emacsclient --eval '(with-scratchpad-name \"scratch-" ++ title ++ "\" " ++ cmd ++ ")'"
    terminalApp s        = NS s (termCmd s s) (title =? ("scratch-" ++ s)) (rectFloating (1/20) (1/16) (2/5) (2/5))


{- XMobar Configuration -}

myxBar :: ScreenId -> IO StatusBarConfig
myxBar n = case n of
  0 -> xbarCfg 0
  1 -> xbarCfg 1
  _ -> mempty -- nothing on the rest of the screens
  where
    xbarCfg n = do
      host <- fmap nodeName getSystemID
      let pt slot = configdir ++ "xb-" ++ host ++ slot ++ ".hs"
          nth = pt $ "-" ++ show n
          fallback = pt "-fallback"
      path <- case n of 0 -> return $ pt ""
                        _ -> doesFileExist nth >>= \r -> return $ if r then nth else fallback
      return $ statusBarPropTo ("_XMONAD_LOG_" ++ if n == 0 then "1" else "N") ("xmobar -x " ++ show n ++ " " ++ path) (pure $ if n == 0 then xbarPP else xbarPP')

xbarPP :: PP
xbarPP  = filterOutWsPP ["NSP", "X"] $   -- hide some workspaces
          xmobarPP                       -- predefined PP for xmobar, see :i PP
          { ppSep      = ""
          , ppOrder    = \(ws:l:t:r) -> ws:l:t:r -- layout/workspace/title/rest
          , ppLayout   = \t -> xmobarColor "#555" "" $ "  <fn=0>" ++  Data.List.drop 18 t ++ "</fn> "
          , ppHidden   = \w -> "<action=xdotool key super+" ++ show ((+1) $ fromJust $ elemIndex w wss) ++ ">" ++ w ++ "</action>"
          , ppTitle    = \t -> "<fc=#558> =></fc>  <fn=1>" ++ xmobarColor "lightskyblue" "" (shorten 150 t) ++ "</fn>"
          }

xbarPP' :: PP
xbarPP' = xbarPP
          { ppTitle    = \t -> "<fc=#558> =></fc>  <fn=1>" ++ xmobarColor "lightskyblue" "" (shorten 30 t) ++ "</fn>"
          }


{- Themes for Prompt/Title -}

myXPConfig :: XPConfig
myXPConfig    = def { position            = Top
                    , font                = "xft:Source Han Sans CN:size=9:bold:antialias=true"
                    , height              = 22
                    , borderColor         = "#000000"
                    , autoComplete        = Just 100000
                    , searchPredicate     = fuzzyMatch
                    , promptKeymap        = M.union defaultXPKeymap $ M.fromList
                                            [ ((controlMask, xK_g), quit)
                                            , ((controlMask, xK_m), setSuccess True >> setDone True)
                                            , ((controlMask, xK_j), setSuccess True >> setDone True)
                                            , ((controlMask, xK_h), deleteString Prev)
                                            , ((controlMask, xK_f), moveCursor Next)
                                            , ((controlMask, xK_b), moveCursor Prev)
                                            , ((controlMask, xK_p), moveHistory W.focusDown')
                                            , ((controlMask, xK_n), moveHistory W.focusUp')
                                            , ((mod1Mask, xK_p), moveHistory W.focusDown')
                                            , ((mod1Mask, xK_n), moveHistory W.focusUp')
                                            , ((mod1Mask, xK_b), moveWord Prev)
                                            , ((mod1Mask, xK_f), moveWord Next)
                                            ]
                    }

noFrillTheme :: Theme
noFrillTheme  = def { decoHeight          = 20
                    , activeColor         = "#111111"
                    , activeTextColor     = "#999999"
                    , activeBorderColor   = "#333333"
                    , inactiveColor       = "#222222"
                    , inactiveTextColor   = "#444444"
                    , inactiveBorderColor = "#222222"
                    , fontName            = "xft:Source Han Sans CN:size=9:bold:antialias=true"
                    }


{- Utils and Others -}

wmIcon = stringProperty "WM_ICON_NAME"
wmRole = stringProperty "WM_WINDOW_ROLE"

scratchpadOpen = namedScratchpadAction scratchpads

isFloat :: Window -> X Bool
isFloat w = withWindowSet (return . W.floating) >>= \fls -> return (w `M.member` fls)

whenFloat :: Window -> (Window -> X ()) -> X ()
whenFloat w f = isFloat w >>= \is -> if is then f w else notifySend "floating?" 1000

withFocusedFloat :: (Window -> X()) -> X ()
withFocusedFloat f = withFocused (`whenFloat` f)

doFloatToCenter        = placeHook (withGaps (40,20,20,20) simpleSmart) <+> doFloat
doFloatRectPos a b x y = doRectFloat (W.RationalRect a b x y) -- <+> doF W.shiftMaster

floatAll w             = gets (W.floating . windowset) >>= \floats -> if w `M.member` floats then sinkAll else withAll float
floatCurrent w         = windows (\ws -> if M.member w (W.floating ws) then W.sink w ws else W.float w (W.RationalRect (1/3) (1/4) (1/2) (3/5)) ws)

moveAndResize p s w    = whenX (isClient w) $ withDisplay $ \d -> do
    io $ resizeWindow d w `uncurry` s
    io $ moveWindow d w `uncurry` p
    float w

mouseMoveFloat :: Window -> X ()
mouseMoveFloat w = whenFloat w (\it -> focus it >> Flex.mouseWindow Flex.position it >> windows W.shiftMaster)

mouseResizeFloat :: Window -> X ()
mouseResizeFloat w = whenFloat w (\it -> focus it >> Flex.mouseWindow Flex.resize it >> windows W.shiftMaster)

notifySend :: String -> Integer -> X ()
notifySend notification timeout = spawn
          $ "DISPLAY=:0 dunstify -r 28172 -t " ++ show timeout ++ " \"" ++ notification ++ "\""

notifySpawn :: String -> Integer -> X ()
notifySpawn shellCommand = notifySend $ "$(" ++ shellCommand ++ ")"

myGreedyView :: (Eq s, Eq i) => i -> W.StackSet i l a s sd -> W.StackSet i l a s sd
myGreedyView w ws
  | any wTag (W.hidden ws) = W.view w ws
  | (Just s) <- find (wTag . W.workspace) (W.visible ws)
                       = ws { W.current = (W.current ws) { W.workspace = W.workspace s }
                            , W.visible = s { W.workspace = W.workspace (W.current ws) }
                                        : filter (not . wTag . W.workspace) (W.visible ws) }
  | otherwise = ws
  where wTag = (w == ) . W.tag

myFindM :: (Monad m) => (a -> m Bool) -> a -> [a] -> m a
myFindM f def = aux where
    aux []     = return def
    aux (x:xs) = f x >>= \r -> if r then return x else aux xs
