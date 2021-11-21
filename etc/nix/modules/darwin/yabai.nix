{ config, pkgs, ... }:

{
  services = {
    yabai = {
      enable = true;
      package = pkgs.yabai;

      config = {
        layout = "bsp";
        window_placement = "second_child";
        top_padding = 48;
        bottom_padding = 18;
        left_padding = 18;
        right_padding = 18;
        window_gap = 18;
        window_shadow = "on";
        window_border = "off";
        wintow_topmost = "on";
      };

      extraConfig = ''
        yabai -m rule --add app='^System Information$' manage=off
        yabai -m rule --add app='^System Preferences$' manage=off
        yabai -m rule --add title='Preferences$' manage=off
        yabai -m rule --add title='Settings$' manage=off
        yabai -m rule --add title='SentinelOne$' manage=off

        sudo yabai --load-sa
        yabai -m signal --add event=dock_did_restart action="sudo yabai --load-sa"
      '';
    };

    skhd = {
      enable = true;
      package = pkgs.skhd;

      skhdConfig = ''
        cmd + shift - return : open -n /Applications/Alacritty.app

        cmd + alt - left : yabai -m window --focus west
        cmd + alt - down : yabai -m window --focus south
        cmd + alt - up : yabai -m window --focus north
        cmd + alt - right : yabai -m window --focus east

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
      '';
    };

    spacebar = {
      enable = true;
      package = pkgs.spacebar;
      config = {
        position = "top";
        height = 28;
        title = "on";
        spaces = "on";
        clock = "off";
        power = "on";
        padding_left = 20;
        padding_right = 20;
        spacing_left = 25;
        spacing_right = 25;
        text_font = ''"Monoid Nerd Font Mono:Retina:14.0"'';
        icon_font = ''"Monoid Nerd Font Mono:Retina:14.0"'';
        background_color = "0xff2E3440";
        foreground_color = "0xffD8DEE9";
        space_icon_color = "0xff8fBcBB";
        power_icon_color = "0xffEBCBDB";
        battery_icon_color = "0xffA3BE8C";
        dnd_icon_color = "0xffA3Be8C";
        clock_icon_color = "0xff81a1C1";
        power_icon_strip = " ";
        space_icon_strip = "I II III IV V VI VII VIII IX X";
        spaces_for_all_displays = "on";
        display_separator = "on";
        display_separator_icon = "";
        clock_icon = "";
        dnd_icon = "";
        clock_format = ''"%d/%m/%y %R"'';
        right_shell = "on";
        right_shell_icon = "";
        right_shell_command = "whoami";
      };
    };
  };
}
