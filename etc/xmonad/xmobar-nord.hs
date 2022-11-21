Config {  font             = "xft:Iosevka Etoile Custom:style=Bold:pixelsize=18"
       , additionalFonts  = [ "xft:Font Awesome 6 Free Solid:pixelsize=18"
                            , "xft:Font Awesome 6 Brands:pixelsize=18"
                            ]
       , bgColor = "#2E3440"
       , fgColor = "#D8DEE9"
       , alpha = 255
       , lowerOnStart = True
       , pickBroadest = False
       , persistent = True
       , hideOnStart = False
       , allDesktops = True
       , overrideRedirect = True
       , position = TopH 42
       , iconRoot = ".config/xmonad/xpm"
       , commands = [ Run Date "%a %b %_d %Y [%H:%M]" "date"  10
                    , Run Cpu ["-L","3","-H","50", "--normal","#88C0D0","--high","#BF616A"] 10
                    , Run Memory ["-t","Mem: <usedratio>%"] 10
                    , Run Com ".config/xmonad/trayer-padding-icon.sh" [] "trayerpad" 20
                    , Run UnsafeXMonadLog
                    ]
       , sepChar = "%"
       , alignSep = "}{"
       , template = " <icon=haskell_20.xpm/> <fc=#4C566A>|</fc> %UnsafeXMonadLog%}{%memory% <fc=#4C566A>|</fc> %cpu% <fc=#4C566A>|</fc> %date% <fc=#4C566A>|</fc> %trayerpad%"
       }

