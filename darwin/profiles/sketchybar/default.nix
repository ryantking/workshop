{ config, lib, ... }:

let
  inherit (config.home) cacheHome;
  inherit (builtins) map concatLists concatStringsSep hasAttr;
  inherit (lib) mergeAttrs pipe mapAttrs mapAttrsToList isBool optional;

  mkSettings = settings:
    pipe settings [
      (mapAttrs (_: v: if isBool v then (if v then "on" else "off") else toString v))
      (mapAttrsToList (k: v: "${k}=${v}"))
      (concatStringsSep " \\\n\t\t")
    ];

  mkItem = item:
    let
      type = if (hasAttr "type" item) then item.type else "item";
      action = if (type == "clone") then "--clone" else "--add ${type}";
    in
    concatStringsSep " \\\n" (concatLists [
      (optional (type != "set") "\t${action} \"${item.name}\" ${concatStringsSep " \\\n\t\t" item.args}")
      [
        "\t--set \"${item.name}\""
        "\t\t${mkSettings item.settings}"
      ]
      (optional (hasAttr "events" item) "\t--subscribe ${item.name} ${concatStringsSep " " item.events}")
    ]);

  mkConfig = { bar, default, events, items }:
    let
      bar' = "\t--bar \\\n\t\t${mkSettings bar}";
      default' = "\t--default \\\n\t\t${mkSettings default}";
      events' = map (event: "\t--add event ${event}") events;
      items' = map mkItem items;
    in
    ''#!/usr/bin/env dash

HAS_BATTERY=$(if [ "$(pmset -g batt | grep "Battery")" = "" ]; then echo "false"; else echo "true"; fi)

${concatStringsSep " \\\n" (["sketchybar" bar' default'] ++ events' ++ items')}

sketchybar --update

echo "sketchybar configuration loaded.."'';

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
  homebrew = {
    taps = [ "FelixKratz/formulae" ];
    brews = [ "sketchybar" "ifstat" ];
    casks = [ "font-sf-pro" "sf-symbols" ];
  };

  system.defaults.NSGlobalDomain._HIHideMenuBar = true;

  services.yabai.config.external_bar = "main:32:0";

  home.configFile = with barConfig; {
    "sketchybar/sketchybarrc" = {
      executable = true;
      text = mkConfig ((import ./core.nix { inherit barConfig; }) // {
        events = [ "window_focus" "monocle" "battery" "wifi" "brew_upgrade" "git_push" "spotify_change ${spotifyEvent}" ];
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
      });
    };

    "sketchybar/colors.sh".text = with colors; ''
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
  };

  launchd.user.agents."sketchybar".serviceConfig = {
    ProgramArguments = [ "/usr/local/bin/sketchybar" ];
    EnvironmentVariables.PATH = config.environment.systemPath;
    RunAtLoad = true;
    KeepAlive = true;
    ProcessType = "Interactive";
    Nice = -20;
    StandardOutPath = "${cacheHome}/logs/sketchybar.out.log";
    StandardErrorPath = "${cacheHome}/logs/sketchybar.err.log";
  };
}
