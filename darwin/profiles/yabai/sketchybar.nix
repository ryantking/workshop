{ self, config, lib, ... }:

let
  inherit (builtins) concatStringsSep concatLists getAttr hasAttr foldl' elemAt;
  inherit (config.home) cacheHome;
  inherit (lib) isBool;
  inherit (lib.lists) optional optionals imap1;
  inherit (lib.attrsets) mapAttrs mapAttrsToList;
  inherit (lib.trivial) pipe;

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

  mkSpace = name: icon: { inherit name icon; };

  mkSpaces = displays:
    let
      mkSpacesForDisplay = displayNdx: spaces:
        let
          mkSpaceLabel = n: {
            name = "spaces_${toString n}.label";
            type = "clone";
            args = [ "label_template" ];
            settings = {
              label = "spc";
              "label.width" = 48;
              "label.align" = "center";
              associated_display = n;
              position = "left";
              drawing = true;
            };
          };

          mkSpaceIcon = n: space:
            let
              spaceColors = with colors; [ none green yellow orange red magenta blue ];
            in
            {
              inherit (space) name;
              type = "clone";
              args = [ "space_template" ];
              settings = {
                inherit (space) icon;
                associated_space = ((displayNdx - 1) * 6) + n;
                "icon.highlight_color" = elemAt spaceColors n;
                "icon.background.color" = elemAt spaceColors n;
                drawing = true;
              };
            };

          mkSpaceBracket = n: spaces: {
            name = "spaces_${toString n}";
            type = "bracket";
            args = [ "spaces_${toString n}.label" ] ++ (map (getAttr "name") spaces);
            settings = {
              "background.drawing" = true;
            };
          };
        in
        concatLists [
          [
            {
              name = "space_template";
              type = "set";
              settings = {
                associated_display = displayNdx;
              };
            }
            (mkSpaceLabel displayNdx)
          ]
          (imap1 mkSpaceIcon spaces)
          [ (mkSpaceBracket displayNdx spaces) ]
        ];
    in
    [
      {
        name = "space_template";
        type = "space";
        args = [ "left" ];
        settings = {
          "icon.highlight_color" = colors.green;
          "label.drawing" = false;
          drawing = false;
          updates = true;
          "label.font" = ''"${font}:Black:13.0"'';
          "icon.font" = ''"${font}:Bold:17.0"'';
          script = ''"${pluginDir}/space.sh"'';
          "icon.padding_right" = 6;
          "icon.padding_left" = 3;
          "background.padding_left" = 2;
          "background.padding_right" = 2;
          "icon.background.height" = 2;
          "icon.background.color" = colors.fg;
          "icon.background.y_offset" = -12;
          click_script = spaceClickScript;
        };
      }
    ] ++ concatLists (imap1 mkSpacesForDisplay displays);

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

  font = "SF Pro";
  loadingIcon = "􀖇";
  pluginDir = "${config.workshop.configDir}/sketchybar";
  spaceClickScript = ''"yabai -m space --focus \$SID"'';
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
in
{
  homebrew = {
    taps = [ "FelixKratz/formulae" ];
    brews = [ "sketchybar" "ifstat" ];
    casks = [ "font-sf-pro" "sf-symbols" ];
  };

  system.defaults.NSGlobalDomain._HIHideMenuBar = true;

  services.yabai.config.external_bar = "main:32:0";

  home.configFile."sketchybar/sketchybarrc" = {
    executable = true;
    text = mkConfig {
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
      events = [ "window_focus" "monocle" "battery" "wifi" ];
      items = [
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
        {
          name = "apple.logo";
          args = [ "left" ];
          settings = {
            icon = "􀣺";
            "icon.font" = ''"${font}:Black:16.0"'';
            click_script = ''"sketchybar -m --set \$NAME popup.drawing=toggle"'';
            "label.drawing" = false;
            "background.padding_right" = 0;
          };
        }
        {
          name = "apple.preferences";
          args = [ "popup.apple.logo" ];
          settings = {
            icon = "􀺽";
            label = "Preferences";
            click_script = ''"open -a 'System Preferences'; sketchybar -m --set apple.logo popup.drawing=off"'';
          };
        }
        {
          name = "apple.activity";
          args = [ "popup.apple.logo" ];
          settings = {
            icon = "􀒓";
            label = "Activity";
            click_script = ''"open -a 'Activity Monitor'; sketchybar -m --set apple.logo popup.drawing=off"'';
          };
        }
        {
          name = "apple.lock";
          args = [ "popup.apple.logo" ];
          settings = {
            icon = "􀒳";
            label = ''"Lock Screen"'';
            click_script = ''"pmset displaysleepnow; sketchybar -m --set apple.logo popup.drawing=off"'';
          };
        }
        {
          name = "system.label";
          type = "clone";
          args = [ "label_template" ];
          settings = {
            label = "sys";
            position = "left";
            drawing = true;
            script = ''"${pluginDir}/window_title.sh"'';
          };
          events = [ "front_app_switched" ];
        }
        {
          name = "Control Center,Battery";
          type = "alias";
          args = [ "left" ];
          settings = {
            update_freq = 2;
            drawing = "$HAS_BATTERY";
            "icon.padding_left" = -5;
            "label.drawing" = false;
            "background.padding_left" = 0;
            "background.padding_right" = -3;
            click_script = ''"sketchybar -m --set \"\$NAME\" popup.drawing=toggle; sketchybar --trigger battery"'';
          };
        }
        {
          name = "battery.details";
          args = [ ''popup."Control Center,Battery"'' ];
          settings = {
            updates = "$HAS_BATTERY";
            script = ''"${pluginDir}/battery.sh"'';
            "label.padding_right" = 8;
          };
          events = [ "battery" ];
        }
        {
          name = "Control Center,WiFi";
          type = "alias";
          args = [ "left" ];
          settings = {
            update_freq = 3;
            "icon.drawing" = false;
            "label.drawing" = false;
            "background.padding_left" = -4;
            "background.padding_right" = -4;
            click_script = ''"sketchybar -m --set \"\$NAME\" popup.drawing=toggle; sketchybar --trigger wifi"'';
          };
        }
        {
          name = "wifi.details";
          args = [ ''popup."Control Center,WiFi"'' ];
          settings = {
            updates = true;
            script = ''"${pluginDir}/wifi.sh"'';
            "label.padding_right" = 5;
          };
          events = [ "wifi" ];
        }
        {
          name = "Control Center,Sound";
          type = "alias";
          args = [ "left" ];
          settings = {
            update_freq = 2;
            "icon.drawing" = false;
            "label.drawing" = false;
            "background.padding_left" = -4;
            "background.padding_right" = -4;
          };
        }
        {
          name = "system.mic";
          args = [ "left" ];
          settings = {
            "update_freq" = 100;
            "label.drawing" = false;
            script = ''"${pluginDir}/mic.sh"'';
            click_script = ''"${pluginDir}/mic_click.sh"'';
          };
        }
        {
          name = "system.caffeinate";
          args = [ "left" ];
          settings = {
            update_freq = 100;
            icon = loadingIcon;
            "label.drawing" = false;
            script = ''"${pluginDir}/caffeinate.sh"'';
          };
          events = [ "mouse.clicked" ];
        }
        {
          name = "system.yabai";
          args = [ "left" ];
          settings = {
            script = ''"${pluginDir}/yabai.sh"'';
            "icon.font" = ''"${font}:Bold:16.0"'';
            "label.drawing" = false;
            updates = true;
          };
          events = [ "window_focus" "monocle" "mouse.clicked" ];
        }
        {
          name = "system";
          type = "bracket";
          args = [
            "system.label"
            ''"Control Center,Battery"''
            ''"Control Center,WiFi"''
            ''"Control Center,Sound"''
            "system.mic"
            "system.caffeinate"
            "system.yabai"
          ];
          settings = {
            "background.drawing" = true;
          };
        }
      ] ++ (mkSpaces [
        [
          (mkSpace "code" "􀤙")
          (mkSpace "web1" "􀆪")
          (mkSpace "term" "􀩼")
          (mkSpace "work" "􀷾")
          (mkSpace "chat" "􀕻")
          (mkSpace "misc1" "􀍠")
        ]
        [
          (mkSpace "music" "􀑈")
          (mkSpace "web2" "􀆪")
          (mkSpace "misc2" "􀍠")
        ]
      ]) ++ [
      ];
    };
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
