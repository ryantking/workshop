Config {  font             = "xft:Iosevka Etoile Custom:style=bold:pixelsize=18"
       , additionalFonts  = [ "xft:Font Awesome 6 Free Solid:pixelsize=18"
                            , "xft:Font Awesome 6 Brands:pixelsize=18"
                            ]
       , bgColor = "#2E3440"
       , fgColor = "#D8DEE9"
       , border = BottomB
       , borderWidth = 2
       , borderColor = "#88C0D0"
       , alpha = 255
       , lowerOnStart = True
       , pickBroadest = False
       , persistent = True
       , hideOnStart = False
       , allDesktops = True
       , overrideRedirect = True
       , position = TopH 42
       , iconRoot = ".config/xmonad/xpm"
       , commands = [ Run Date "%H:%M" "date"  10
                    , Run Cpu
                      ["-t", "<fc=#5E81AC>CPU</fc> <total>%"
                      , "-L", "3"
                      , "-H", "50"
                      , "--high", "#BF616A"
                      ] 10
                    , Run Memory
                      ["-t","<fc=#5E81AC>MEM</fc> <usedratio>%"] 10
                    , Run Alsa "default" "Master"
                      ["-t", "<fc=#5E81AC>VOL</fc> <volume>%"]
                    , Run DynNetwork
                      ["-t", "<fc=#88C0D0><dev></fc> <rxipat> <fc=#5E81AC>DN</fc> <rx> <fc=#4C566A>KB/s</fc> <fc=#5E81AC>UP</fc> <tx> <fc=#4C566A>KB/s</fc>"] 10
                    , Run Com ".config/xmonad/trayer-padding-icon.sh" [] "trayerpad" 20
                    , Run UnsafeXMonadLog
                    ]
       , sepChar = "%"
       , alignSep = "}{"
       , template = " %UnsafeXMonadLog%}{%alsa:default:Master%  <fc=#4C566A>|</fc>  %memory%  <fc=#4C566A>|</fc>  %cpu%  <fc=#4C566A>|</fc>  %dynnetwork%  <fc=#4C566A>|</fc>  %date%  <fc=#4C566A>|</fc>%trayerpad%"
       }

