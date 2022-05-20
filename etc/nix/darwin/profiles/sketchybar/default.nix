{ config
, lib
, ...
}:
let
  font = "SF Pro";
  loadingIcon = "􀖇";
  pluginDir = "${config.workshop.configHome}/sketchybar";
  spotifyEvent = "com.spotify.client.PlaybackStateChanged";
  padding = 3;

  colors =
    let
      inherit (config.colorscheme) colors;
    in
    {
      fg = "0xFF${colors.base04}";
      bg = "0xFF${colors.base00}";
      fgAlt = "0xFF${colors.base06}";
      bgAlt = "0xFF${colors.base03}";
      black = "0xFF${colors.base01}";
      white = "0xFF${colors.base05}";
      red = "0xFF${colors.base08}";
      orange = "0xFF${colors.base09}";
      yellow = "0xFF${colors.base0A}";
      green = "0xFF${colors.base0B}";
      cyan = "0xFF${colors.base0C}";
      blue = "0xFF${colors.base0D}";
      magenta = "0xFF${colors.base0E}";
      none = "0x00000000";
    };

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

  settings = {
    inherit lib font loadingIcon pluginDir spotifyEvent padding colors segment shadow;
  };
in
{
  services.sketchybar = {
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

    items = builtins.concatLists [
      [
        {
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
        }
      ]
      (import ./menu.nix settings)
      (import ./system.nix settings)
      (import ./spaces.nix settings)
      (import ./github.nix settings)
      (import ./calendar.nix settings)
      (import ./cpu.nix settings)
      (import ./spotify.nix settings)
    ];
  };

  home.configFile."sketchybar/colors.sh".text = ''
    COLOR_FG=${colors.fg}
    COLOR_BG=${colors.bg}
    COLOR_FG_ALT=${colors.fgAlt}
    COLOR_BG_ALT=${colors.bgAlt}
    COLOR_BLACK=${colors.black}
    COLOR_RED=${colors.red}
    COLOR_GREEN=${colors.green}
    COLOR_YELLOW=${colors.yellow}
    COLOR_BLUE=${colors.blue}
    COLOR_MAGENTA=${colors.magenta}
    COLOR_CYAN=${colors.cyan}
    COLOR_WHITE=${colors.white}
  '';
}
