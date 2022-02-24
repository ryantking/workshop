{ pkgs, ... }:

{
  services.skhd = {
    enable = true;
    package = pkgs.skhd;

    skhdConfig = ''
      cmd + shift - return : open -na /Applications/Alacritty.app
      hyper - e : emacs

      cmd + alt - left : yabai -m window --focus west || (yabai -m display --focus west)
      cmd + alt - down : yabai -m window --focus south || (yabai -m display --focus south)
      cmd + alt - up : yabai -m window --focus north || (yabai -m display --focus north)
      cmd + alt - right : yabai -m window --focus east || (yabai -m display --focus east)

      cmd + alt + shift - left : yabai -m window --swap west || (yabai -m window --display west; yabai -m display --focus west)
      cmd + alt + shift - down : yabai -m window --swap north || (yabai -m window --display north; yabai -m display --focus north)
      cmd + alt + shift - up : yabai -m window --swap south || (yabai -m window --display south; yabai -m display --focus south)
      cmd + alt + shift - right : yabai -m window --swap east || (yabai -m window --display east; yabai -m display --focus east)

      cmd + alt - n : yabai -m space --create && \
            index="(yabai -m query --spaces --display | jq 'map(select(.\"native-fullscreen\" == 0))[-1].index')" && \
            yabai -m space --focus "''${index}"

      cmd + alt - w : yabai -m space --destroy
      cmd + alt - x : yabai -m space --focus recent
      cmd + alt - z : yabai -m space --focus prev
      cmd + alt - c : yabai -m space --focus next
      cmd + alt - 1 : yabai -m space --focus 1
      cmd + alt - 2 : yabai -m space --focus 2
      cmd + alt - 3 : yabai -m space --focus 3
      cmd + alt - 4 : yabai -m space --focus 4
      cmd + alt - 5 : yabai -m space --focus 5
      cmd + alt - 6 : yabai -m space --focus 6
      cmd + alt - 7 : yabai -m space --focus 7
      cmd + alt - 8 : yabai -m space --focus 8
      cmd + alt - 9 : yabai -m space --focus 9
      cmd + alt - 0 : yabai -m space --focus 10

      cmd + alt + shift - x : yabai -m window --space recent; yabai -m space --focus recent
      cmd + alt + shift - z : yabai -m window --space prev; yabai -m space --focus prev
      cmd + alt + shift - c : yabai -m window --space next; yabai -m space --focus next
      cmd + alt + shift - 1 : yabai -m window --space  1; yabai -m space --focus 1
      cmd + alt + shift - 2 : yabai -m window --space  2; yabai -m space --focus 2
      cmd + alt + shift - 3 : yabai -m window --space  3; yabai -m space --focus 3
      cmd + alt + shift - 4 : yabai -m window --space  4; yabai -m space --focus 4
      cmd + alt + shift - 5 : yabai -m window --space  5; yabai -m space --focus 5
      cmd + alt + shift - 6 : yabai -m window --space  6; yabai -m space --focus 6
      cmd + alt + shift - 7 : yabai -m window --space  7; yabai -m space --focus 7
      cmd + alt + shift - 8 : yabai -m window --space  8; yabai -m space --focus 8
      cmd + alt + shift - 9 : yabai -m window --space  9; yabai -m space --focus 9
      cmd + alt + shift - 0 : yabai -m window --space 10; yabai -m space --focus 10

      cmd + ctrl - x  : yabai -m display --focus recent
      cmd + ctrl - z  : yabai -m display --focus prev
      cmd + ctrl - c  : yabai -m display --focus next
      cmd + ctrl - 1  : yabai -m display --focus 1
      cmd + ctrl - 2  : yabai -m display --focus 2

      cmd + ctrl + shift - x  : yabai -m window --display recent; yabai -m display --focus recent
      cmd + ctrl + shift - z  : yabai -m window --display prev; yabai -m display --focus prev
      cmd + ctrl + shift - c  : yabai -m window --display next; yabai -m display --focus next
      cmd + ctrl + shift - 1  : yabai -m window --display 1; yabai -m display --focus 1
      cmd + ctrl + shift - 2  : yabai -m window --display 2; yabai -m display --focus 2
      cmd + ctrl + shift - 3  : yabai -m window --display 3; yabai -m display --focus 3

      hyper - t : yabai -m window --toggle float;\
                  yabai -m window --grid 4:4:1:1:2:2
    '';
  };
}
