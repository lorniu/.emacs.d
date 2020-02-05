Config {
  font               =   "xft:Bitstream Vera Sans Mono:size=8:normal:antialias=true,Source Han Sans CN:size=8:normal:antialias=true"
  , additionalFonts  = [ "xft:Bitstream Vera Sans Mono:size=9:normal:antialias=true,Source Han Sans CN:size=9:normal:antialias=true",
                         "xft:Bitstream Vera Sans Mono:size=9:bold:antialias=true,Source Han Sans CN:size=9:bold:antialias=true",
                         "xft:Bitstream Vera Sans Mono:size=10:bold:antialias=true,Source Han Sans CN:size=10:bold:antialias=true" ]
  , bgColor          = "black"
  , fgColor          = "#888888"
  , alpha            = 255
  , position         = Top -- Top, TopP, TopW, TopSize, Bottom, BottomP, BottomW, BottomSize or Static
  , border           = BottomB
  , borderWidth      = 0
  , borderColor      = "red"

  , lowerOnStart     = True    -- send to bottom of window stack on start
  , hideOnStart      = False   -- start with window unmapped (hidden)
  , allDesktops      = True    -- show on all desktops
  , overrideRedirect = False   -- set the Override Redirect flag (Xlib)
  , pickBroadest     = False   -- choose widest display (multi-monitor)
  , persistent       = True    -- enable/disable hiding (True = disabled)

  , commands =
      [ Run UnsafeStdinReader
      , Run DynNetwork [ "-t<fc=#66f><dev></fc>: <tx>kB/s ⇅ <rx>kB/s", "-L 100000", "-H 500000", "-l", "lightgreen", "-n", "orange", "-h", "red" ] 10
      , Run Cpu        [ "-t<fc=#66f>Cpu</fc>: <total>%", "-m 2", "-L 50", "-H 85", "-l", "lightgreen", "-n", "darkorange", "-h", "darkred" ] 10
      , Run Memory     [ "-t<fc=#66f>Mem</fc>: <usedratio>%", "-L 20", "-H 90", "-l", "lightgreen", "-n", "darkorange", "-h", "darkred" ] 10
      --, Run Swap       [ "-t<usedratio>%" ] 10
      , Run Date       "<fc=#ccc><fn=1>%F</fn></fc> (%a) <fc=#fff><fn=2>%T</fn></fc>" "date" 10
      , Run CoreTemp   [ "--template" , "<core0>°C"
                       , "--High"     , "80"
                       , "--high"     , "darkred"
                       ] 30
      , Run Battery    [ "--template" , "<acstatus>"
                       , "--Low"      , "20"
                       , "--High"     , "80"
                       , "--low"      , "red"
                       , "-S"         , "True"
                       , "--"
                       , "-o"         , "Battery (<timeleft> ~ <left>)"
                       , "-O"         , "<fc=#006000><left></fc>"
                       , "-i"         , "<left>"
                       ] 10
      -- Run Com        "/usr/bin/dropbox" ["status"] "dropbox" 50000
      ]
  , sepChar  = "%" , alignSep = "}{"
  , template = "<fn=1>%UnsafeStdinReader%</fn>  }{ %dynnetwork% | %memory%  %cpu% (%coretemp%) | %date% | %battery% "
  }
