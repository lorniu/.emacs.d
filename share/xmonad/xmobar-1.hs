Config {
    font             = "xft:Bitstream Vera Sans Mono:size=8:normal:antialias=true,Source Han Sans CN:size=8:normal:antialias=true"
  , bgColor          = "#111"
  , fgColor          = "#eee"
  , alpha            = 88
  , position         = Top

  , lowerOnStart     = True
  , hideOnStart      = False
  , allDesktops      = True
  , overrideRedirect = False
  , pickBroadest     = False
  , persistent       = True

  , commands = [ Run UnsafeStdinReader , Run Date "<fc=#ccc>%F</fc> (%a) <fc=#fff>%T</fc>" "date" 10 ]
  , sepChar  = "%" , alignSep = "}{"
  , template = "%UnsafeStdinReader% }{ %date% "
  }
