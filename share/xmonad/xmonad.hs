--
-- Author: imfine
-- Sample: /usr/share/xmonad/man/xmonad.hs
-- Document: http://hackage.haskell.org/package/xmonad-contrib-0.13/docs/XMonad-Doc-Extending.html
--
---- Installation
--
-- git clone https://github.com/xmonad/xmonad          # cabal install
-- git clone https://github.com/xmonad/xmonad-contrib  # cabal install --lib
-- git clone https://github.com/jaor/xmobar            # cabal install -f with_xft --enable-debug-info=1
--
-- dmenu                  : yes, menu
-- xdotool                : click event
-- xcompmgr               : shadow effects and more
-- tilix                  : terminal
-- nemo                   : file manager
-- slock                  : screen saver | xautolock -time 15 -locker slock
-- deepin-screen-recorder : screenshot
-- feh                    : image viewer
-- dunst                  : notification
-- acpilight              : xbacklight
--
-- removed: gnome-screenshot/gscreenshot/scrot/pcmanfm/xfce4-terminal
--

import Data.Char
import Data.List
import Data.Maybe
import Data.Time
import System.Exit
import System.IO
import System.Posix.Unistd
import System.Environment
import System.Directory (getHomeDirectory)
import qualified Data.Map as M
import System.Directory

import XMonad
import XMonad.Actions.CycleWS (nextScreen)
import XMonad.Actions.WithAll (withAll, sinkAll)
import XMonad.Actions.SpawnOn (manageSpawn)
import XMonad.Actions.SwapWorkspaces (swapTo)
import XMonad.Actions.MouseResize (mouseResize)
import XMonad.Actions.Minimize (withLastMinimized, minimizeWindow, maximizeWindowAndFocus)
import XMonad.Actions.CycleWS
import XMonad.Actions.WindowGo
import XMonad.Actions.UpdatePointer
import qualified XMonad.Actions.FlexibleResize as Flex
import XMonad.Hooks.SetWMName (setWMName)
import XMonad.Hooks.Place (placeHook, withGaps, simpleSmart)
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageHelpers
import XMonad.Hooks.ManageDocks (avoidStruts, docks, manageDocks, ToggleStruts(..))
import XMonad.Hooks.PositionStoreHooks
import XMonad.Prompt
import XMonad.Prompt.Input
import XMonad.Prompt.Window
import XMonad.Prompt.FuzzyMatch (fuzzyMatch)
import XMonad.Util.NamedScratchpad
import XMonad.Util.Run
import XMonad.Util.Cursor (setDefaultCursor)
import XMonad.Util.SpawnOnce (spawnOnce)
import XMonad.Util.EZConfig (additionalKeys, additionalKeysP, additionalMouseBindings)
import qualified XMonad.StackSet as W

import XMonad.Layout.SimpleDecoration
import XMonad.Layout.NoFrillsDecoration
import XMonad.Layout.LayoutModifier
import XMonad.Layout.Named (named)
import XMonad.Layout.Spacing (spacingRaw, Border(..))
import XMonad.Layout.Minimize (minimize)
import XMonad.Layout.Maximize (maximizeWithPadding, maximizeRestore)
import XMonad.Layout.NoBorders (smartBorders, noBorders)
import XMonad.Layout.PerWorkspace (onWorkspace)
import XMonad.Layout.WindowArranger     -- move/resize windows with keyboard
import XMonad.Layout.ToggleLayouts      -- toggle between two layouts
import XMonad.Layout.PositionStoreFloat -- floating layout with dual-head setup
import XMonad.Layout.MagicFocus (followOnlyIf, disableFollowOnWS) -- disable floated WS's follow
import XMonad.Layout.BoringWindows (boringWindows, focusDown, focusUp, focusMaster) -- when focus change, ignore minimized windows
import XMonad.Layout.IndependentScreens (countScreens)

import XMonad.Layout.Grid
import XMonad.Layout.Circle
import XMonad.Layout.OneBig
import XMonad.Layout.ThreeColumns
import XMonad.Layout.MouseResizableTile
import XMonad.Layout.BinarySpacePartition



main :: IO ()
main = do
  -- log
  now <- fmap show getZonedTime; trace $ "\n---- " ++ now ++ "---- \n"

  -- xmobar
  xmprocs <- xmobarProcs

  -- wallpaper
  spawn =<< fmap ("feh --bg-scale " ++) myWallpaper

  -- host
  host <- fmap nodeName getSystemID

  case myLayout host
    of Layout c -> xmonad $ docks def {
         modMask              = myModMask
         , terminal           = myTerminal
         , workspaces         = ws

         , borderWidth        = 1
         , focusedBorderColor = "lightblue"
         , normalBorderColor  = "#555555"
         , focusFollowsMouse  = False
         , clickJustFocuses   = False

         , startupHook        = myStartup      -- called after started
         , layoutHook         = c              -- called when workspace loaded
         , manageHook         = myManage       -- called when a new window is created.
         , logHook            = myLogs xmprocs -- called when the stack of windows managed has been changed.
         , handleEventHook    = myEvents       -- called on all events handled by xmonad.
         }
         `additionalKeys` myKeys
         `additionalKeysP` myKeysP
         `additionalMouseBindings` [ ((myModMask, button3), (\w -> focus w >> Flex.mouseResizeWindow w)) ]



myModMask       = mod4Mask

myTerminal      = "tilix"
fileManager     = "nemo --no-desktop"
nemoTermCfg     = "gsettings set org.cinnamon.desktop.default-applications.terminal exec " ++ myTerminal
nemoTermCfgArg  = "gsettings set org.cinnamon.desktop.default-applications.terminal exec-arg '--title=floater'"

screenSaver     = "slock"
myNotication    = "dunst -conf ~/.emacs.d/share/xmonad/dunst.conf"
screenShotArea  = "deepin-screen-recorder"
screenShotFull  = "deepin-screen-recorder -f"

imageEditor = do
  krita <- findExecutable "krita"
  return $ if krita == Nothing then "gimp" else "krita"

myWallpaper = do
  wp <- lookupEnv "XMONAD_WALLPAPER"
  case wp of
    Just w   -> return w
    Nothing  -> do
      d  <- fmap (++ "/.notes/asset/image/wallpaper.png") getHomeDirectory
      d' <- fmap (++ "/images/wallpaper.png") getHomeDirectory
      f  <- doesFileExist d
      return (if f then d else d')

xmobarProcs = do
  hd <- getHomeDirectory
  n  <- countScreens; trace $ "Monitors total: " ++ show n

  let xmobarHS p = hd ++ "/.emacs.d/share/xmonad/xmobar"
                      ++ (if p == "" then "" else "-" ++ p) ++ ".hs"

  xmobarPaths <- mapM (\i -> do
                          f <- doesFileExist $ xmobarHS (show i)
                          return (if f then xmobarHS (show i) else xmobarHS "")
                      ) [0..n-1]

  mapM (\i -> spawnPipe $ "xmobar " ++ xmobarPaths!!i ++ (" -x " ++ show i)) [0..n-1]

ws = ["1.W", "2.M", "3.T", "④", "5.S", "6", "7", "8:IMG", "9.Misc"]
fworkspace = ws!!3 -- float Me



myStartup :: X ()
myStartup = do
  setWMName "LG3D"
  setDefaultCursor xC_left_ptr
  -- spawnOnce myTerminal
  spawnOnce "xcompmgr"
  spawnOnce nemoTermCfg
  spawnOnce nemoTermCfgArg
  spawnOnce myNotication
  spawnOnce =<< io imageEditor

myKeys :: [((KeyMask, KeySym), X ())]
myKeys  = [ ((m, xK_w)                       , nextScreen)
          , ((m .|. shiftMask, xK_w)         , shiftNextScreen)
          , ((m .|. controlMask, xK_w)       , swapNextScreen)

          , ((m, xK_h)                       , sendMessage Shrink)
          , ((m, xK_l)                       , sendMessage Expand)

          , ((m, xK_Tab)                     , focusDown)
          , ((m .|. shiftMask, xK_Tab)       , focusUp)
          , ((m, xK_j)                       , focusDown)
          , ((m, xK_k)                       , focusUp)
          , ((m, xK_m)                       , focusMaster)

          , ((m .|. shiftMask, xK_m)         , windows W.swapMaster)
          , ((m .|. shiftMask, xK_k)         , windows W.swapUp)
          , ((m .|. shiftMask, xK_j)         , windows W.swapDown)
          , ((m, xK_comma)                   , sendMessage (IncMasterN 1))
          , ((m, xK_period)                  , sendMessage (IncMasterN (-1)))
          , ((m .|. controlMask, xK_Left)    , swapTo Prev)
          , ((m .|. controlMask, xK_Right)   , swapTo Next)

          , ((m, xK_b)                       , sendMessage ToggleStruts)
          , ((m, xK_space)                   , sendMessage ToggleLayout)
          , ((m, xK_Return)                  , withFocused (sendMessage . maximizeRestore))
          , ((m .|. shiftMask, xK_n)         , sendMessage NextLayout)

          , ((m .|. shiftMask, xK_t)         , withFocused floatAll)
          , ((m, xK_t)                       , withFocused floatCur)
          , ((m, xK_d)                       , withFocused minimizeWindow)
          , ((m, xK_f)                       , withLastMinimized maximizeWindowAndFocus)
          , ((m, xK_g)                       , windowPrompt promptTheme Goto allWindows)
          , ((m .|. shiftMask, xK_g)         , windowPrompt promptTheme Bring allWindows)

          , ((m, xK_quoteleft)               , spawn "dmenu_run -l 10")
          , ((m, xK_r)                       , spawn $ myTerminal ++ " --title=floater")
          , ((m, xK_e)                       , spawn fileManager)
          , ((0, xK_Print)                   , spawn screenShotArea)
          , ((controlMask, xK_Print)         , spawn screenShotFull)

          , ((m .|. controlMask .|. shiftMask, xK_t), namedScratchpadAction scratchpads "htop")
          ] ++ wsOpt ++ screenOpt
  where m = myModMask
        floatAll w = do floats <- gets (W.floating . windowset)
                        if w `M.member` floats
                          then sinkAll
                          else withAll float
        floatCur w = windows (\ws ->
                                if M.member w (W.floating ws)
                                  then W.sink w ws
                                  else (W.float w (W.RationalRect (1/3) (1/4) (1/2) (3/5)) ws))
        wsOpt      = [ ((m .|. n, k) , windows $ f i)
                         | (i, k) <- zip ws [xK_1 .. xK_9]
                         , (f, n) <- [(W.greedyView, 0), (W.shift, shiftMask)] ]
        screenOpt  = [ ((m .|. n, k) , screenWorkspace sc >>= flip whenJust (windows . f))
                         | (k, sc) <- zip [xK_F1, xK_F2, xK_F3, xK_F4] [0..]
                         , (f, n) <- [(W.view, 0), (W.shift, shiftMask)]]

myKeysP :: [(String, X ())]
myKeysP = [ ("<XF86MonBrightnessDown>" , spawn "brightnessctl s 2-")
          , ("<XF86MonBrightnessUp>"   , spawn "brightnessctl s 2+")
          , ("<XF86AudioMute>"         , spawn "isend sound toggle")
          , ("<XF86AudioLowerVolume>"  , spawn "isend sound down")
          , ("<XF86AudioRaiseVolume>"  , spawn "isend sound up")
          , ("<XF86AudioPlay>"         , spawn "mpc play")
          , ("<XF86AudioPause>"        , spawn "mpc pause")
          , ("<XF86AudioStop>"         , spawn "mpc stop")
          , ("<XF86AudioPrev>"         , spawn "mpc prev")
          , ("<XF86AudioNext>"         , spawn "mpc next")
          , ("<XF86ScreenSaver>"       , spawn screenSaver)
          ]

-- [m-space]: toggle full; [m-s-n]: switch layout
-- [m-,/m-.]: master area; [m-j/m-k/m-m/m-s-m]: Cursor
myLayout host
  = let decoBasic     = boringWindows
                        . avoidStruts                          -- protect dock
                        . smartBorders
                        . windowArrange
                        . minimize                             -- enable hide
                        . maximizeWithPadding 30               -- enable large overview
        decoFloating  = noBorders
                        . noFrillsDeco shrinkText noFrillTheme -- enable title
                        . mouseResize
                        . maximizeWithPadding 0

        floatAll  = named "Floated" $ decoFloating positionStoreFloat

        full      = named "Full" $ noBorders Full
        normal    = named "Normal" $ mouseResizableTile { draggerType = FixedDragger 1 5 }
        grid      = Grid
        oneBig    = OneBig (3/4) (3/4)
        threeCol  = ThreeColMid 1 (3/100) (1/2)
        bsp       = emptyBSP

        group     = normal ||| oneBig ||| threeCol ||| bsp ||| grid
    in case host of
         _ -> Layout
              $ onWorkspace fworkspace floatAll
              $ decoBasic $ toggleLayouts full group

-- Window created (xprop to view WM info)
myManage = manageDocks     -- reveal Docks
           <+> manageSpawn -- spawnOn/spawnHere
           <+> positionStoreManageHook Nothing
           <+> namedScratchpadManageHook scratchpads
           <+> composeAll
           [ isDialog                         --> doFloat
           , className =? "MPlayer"           --> doFloat
           , className =? "feh"               --> doFloat
           , className =^ "imdd."             --> doFloat
           , title     =? "Emulator"          --> doFloat
           , title     =? "Android Emulator"  --> doFloat
           , role      =? "Preferences"       --> doFloat
           , className =? "Gimp"              --> doShift (ws!!7)
           , className =? "krita"             --> doShift (ws!!7)
           , title     =? "floater"           --> doFloatRectPos (1/300) (0.5-1/200) 0.5 0.5
           , title     =^ "aaa"               --> doFloatToCenter
           , title     =^ "app"               --> doFloatToCenter
           , title     =^ "ImageMagick"       --> doFloatToCenter
           , title     =^ "test"              --> doFloatToCenter
           , resource  =? "desktop_window"    --> doIgnore
           ]
           <+> composeOne
           [ transience
           , isFullscreen                     -?> doFloat
           , resource  =? "desktop_window"    -?> doIgnore
           ]
         where
           q =^ x                 = fmap (x `isPrefixOf`) q
           role                   = stringProperty "WM_WINDOW_ROLE"
           doFloatToCenter        = placeHook (withGaps (40,20,20,20) simpleSmart) <+> doFloat
           doFloatRectPos a b x y = doRectFloat (W.RationalRect a b x y) -- <+> doF W.shiftMaster

myLogs xmprocs = mapM_ (\xmproc -> dynamicLogWithPP $ xmobarTheme xmproc) xmprocs
                 >> updatePointer (0.5, 0.5) (0, 0)

myEvents = followOnlyIf (disableFollowOnWS [fworkspace])
           <+> positionStoreEventHook

scratchpads = [ htop, dict ] where
  htop = NS "htop" "xterm -e htop" (title =? "htop") defaultFloating
  dict = NS "stardict" "stardict" (className =? "Stardict") (customFloating $ W.RationalRect (1/6) (1/6) (2/3) (2/3))



promptTheme  = def { position            = Top
                   , font                = "xft:Sarasa UI SC:size=9:medium:antialias=true"
                   , height              = 22
                   , borderColor         = "#000000"
                   , autoComplete        = Just 100000
                   , searchPredicate     = fuzzyMatch
                   }

xmobarTheme proc = xmobarPP
                   { ppOutput            = \str -> do withFile "/home/vip/xxxxx.log" AppendMode (\h -> hPutStrLn h str)
                                                      hPutStrLn proc str
                       , ppOrder             = \(ws:l:t:r) -> ws:t:l:r
                       , ppSep               = ""
                       , ppTitle             = ("<fc=#558>  =></fc>  " ++ ) . xmobarColor "lightskyblue" "" . shorten 50
                       , ppLayout            = \t -> (xmobarColor "#555" "" $ "  <fn=0>" ++  Data.List.drop 18 t ++ "</fn> ")
                       , ppHidden            = \w -> "<action=xdotool key super+" ++ (show $ (+1) $ fromJust $ elemIndex w ws) ++ ">" ++ w ++ "</action>"
                       }

noFrillTheme = def { decoHeight          = 20
                   , activeColor         = "#111111"
                   , activeTextColor     = "#999999"
                   , activeBorderColor   = "#333333"
                   , inactiveColor       = "#222222"
                   , inactiveTextColor   = "#444444"
                   , inactiveBorderColor = "#222222"
                   , fontName            = "xft:Source Han Sans CN:size=9:bold:antialias=true"
                   }



sudoSpawn :: String -> X()
sudoSpawn command = withPrompt "Password" $ run command
                    where run cmd password = spawn $ concat ["echo ", password, " | sudo -S ", cmd]
                          withPrompt prompt fn = inputPrompt (def { position = Top }) prompt ?+ fn

notifySend :: Integer -> String -> X ()
notifySend expireTime shellCommand = spawn
          $ "DISPLAY=:0 notify-send -t " ++ show expireTime
          ++ " -h string:bgcolor:#000000 "
          ++ " \"$(" ++ shellCommand ++ ")\""

mySpacing x = spacingRaw True (Border 0 x x x) True (Border x x x x) True

doFloatAbsRect :: Rational -> Rational -> Rational -> Rational -> ManageHook
doFloatAbsRect x y width height = do
  win <- ask -- get Window
  q <- liftX (floatLocation win) -- get (ScreenId, W.RationalRect)
  let sid = fst q :: ScreenId
      oirgRect = snd q :: W.RationalRect
      ss2ss ss = -- :: StackSet ... -> StackSet ...
        W.float win newRect ss where
          mapping = map (\s -> (W.screen s, W.screenDetail s)) (c:v) where
            c = W.current ss
            v = W.visible ss
          maybeSD = lookup sid mapping
          scRect  = fmap screenRect maybeSD
          newRect = case scRect of
            Nothing -> oirgRect
            Just (Rectangle x0 y0 w0 h0) ->
              W.RationalRect x' y' w' h' where
                W.RationalRect x1 y1 w1 h1 = oirgRect
                x' = if x0 == 0 then x1 else x / (fromIntegral x0)
                y' = if y0 == 0 then y1 else y / (fromIntegral y0)
                w' = if w0 == 0 then w1 else width / (fromIntegral w0)
                h' = if h0 == 0 then h1 else height / (fromIntegral h0)
  doF ss2ss
