{ config, lib, ... }:

let
  inherit (builtins) concatLists;

  barConfig = {
    font = "SF Pro";
    loadingIcon = "􀖇";
    pluginDir = "${config.workshop.configDir}/sketchybar";
    spotifyEvent = "com.spotify.client.PlaybackStateChanged";
    padding = 3;

    segment = {
      height = 28;
      spacing = 13;
      cornerRadius = 4;
      borderWidth = 0;
    };

    shadow = {
      distance = 3;
      angle = 35;
    };

    colors = with config.colorscheme.colors; {
      fg = "0xFF${base04}";
      bg = "0xFF${base00}";
      fgAlt = "0xFF${base06}";
      bgAlt = "0xFF${base03}";
      black = "0xFF${base01}";
      white = "0xFF${base05}";
      red = "0xFF${base08}";
      orange = "0xFF${base09}";
      yellow = "0xFF${base0A}";
      green = "0xFF${base0B}";
      cyan = "0xFF${base0C}";
      blue = "0xFF${base0D}";
      magenta = "0xFF${base0E}";
      none = "0x00000000";
    };

    icons = {
      loading = "􀖇";
    };
  };
in
{
  services.sketchybar = with barConfig; {
    enable = true;

    bar = {
      height = 32;
      corner_radius = 0;
      border_width = 0;
      border_color = colors.bg;
      margin = 0;
      blur_radius = 30;
      position = "top";
      padding_left = 5;
      padding_right = 5;
      color = colors.bg;
      topmost = false;
      font_smoothing = false;
      y_offset = 0;
      shadow = true;
    };

    default = {
      drawing = true;
      lazy = false;
      updates = "when_shown";
      cache_scripts = true;
      "label.font" = ''"${font}:Semibold:13.0"'';
      "icon.font" = ''"${font}:Bold:14.0"'';
      "icon.color" = colors.fg;
      "label.color" = colors.fg;
      "icon.padding_left" = padding;
      "icon.padding_right" = padding;
      "label.padding_left" = padding;
      "label.padding_right" = padding;
      "background.padding_left" = padding;
      "background.padding_right" = padding;
      "background.color" = colors.none;
      "background.border_color" = colors.none;
      "background.border_width" = segment.borderWidth;
      "background.height" = segment.height;
      "background.corner_radius" = segment.cornerRadius;
      "background.drawing" = false;
      "icon.background.height" = segment.height;
      "icon.background.color" = colors.none;
      "icon.background.corner_radius" = segment.cornerRadius;
      "icon.background.drawing" = false;
      "label.background.color" = colors.none;
      "label.background.border_width" = segment.borderWidth;
      "label.background.border_color" = colors.none;
      "label.background.height" = segment.height;
      "label.background.corner_radius" = segment.cornerRadius;
      "label.background.drawing" = false;
      "popup.background.border_width" = 2;
      "popup.background.corner_radius" = segment.cornerRadius;
      "popup.background.border_color" = colors.bg;
      "popup.background.color" = colors.bgAlt;
      "popup.background.shadow.drawing" = true;
      "icon.shadow.drawing" = true;
      "label.shadow.drawing" = true;
      "alias.shadow.drawing" = true;
      "icon.shadow.color" = colors.bg;
      "label.shadow.color" = colors.bg;
      "alias.shadow.color" = colors.bg;
      "icon.shadow.distance" = shadow.distance;
      "label.shadow.distance" = shadow.distance;
      "alias.shadow.distance" = shadow.distance;
      "icon.shadow.angle" = shadow.angle;
      "label.shadow.angle" = shadow.angle;
      "alias.shadow.angle" = shadow.angle;
      "alias.color" = colors.fg;
    };

    events = [
      "window_focus"
      "monocle"
      "battery"
      "wifi"
      "brew_upgrade"
      "git_push"
      "spotify_change${spotifyEvent}"
    ];

    items = concatLists [
      [{
        name = "label_template";
        args = [ "left" ];
        settings = {
          drawing = false;
          click_script = ''"${pluginDir}/toggle_bracket.sh"'';
          "icon.darwing" = false;
          "label.font" = ''"${font}:Black:12.0"'';
          "label.background.drawing" = true;
          "label.color" = colors.fg;
          "label.padding_right" = 5;
          "label.background.height" = segment.height;
          "background.padding_left" = segment.spacing;
          "background.padding_right" = 0;
        };
      }]
      (import ./menu.nix { inherit barConfig lib; })
      (import ./system.nix { inherit barConfig; })
      (import ./spaces.nix { inherit barConfig lib; })
      (import ./github.nix { inherit barConfig; })
      (import ./calendar.nix { inherit barConfig; })
      (import ./cpu.nix { inherit barConfig; })
      (import ./spotify.nix { inherit barConfig; })
    ];
  };

  home.configFile."sketchybar/colors.sh".text = with barConfig.colors; ''
    COLOR_FG=${fg}
    COLOR_BG=${bg}
    COLOR_FG_ALT=${fgAlt}
    COLOR_BG_ALT=${bgAlt}
    COLOR_BLACK=${black}
    COLOR_RED=${red}
    COLOR_GREEN=${green}
    COLOR_YELLOW=${yellow}
    COLOR_BLUE=${blue}
    COLOR_MAGENTA=${magenta}
    COLOR_CYAN=${cyan}
    COLOR_WHITE=${white}
  '';
}
