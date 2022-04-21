{ config, lib, pkgs, ... }:

with lib.types;

let
  inherit (lib) mkEnableOption mkIf hasAttr concatLists concatStringsSep mapAttrs mapAttrsToList pipe optional isBool;
  inherit (pkgs.lib.our) mkOpt';
  inherit (config.home) cacheHome;

  cfg = config.services.sketchybar;
in
{
  options.services.sketchybar = {
    enable = mkEnableOption "SketchyBar";

    logPath = mkOpt' path "${cacheHome}/logs/sketchybar.log" "Path to write logs to";

    bar = mkOpt' attrs { } "Bar configuration settigns";

    default = mkOpt' attrs { } "Default item configuration values";

    events = mkOpt' (listOf str) [ ] "Events to add listeners for";

    items = mkOpt' (listOf attrs) [ ] "Bar items";
  };

  config = mkIf cfg.enable {
    home.configFile = {
      "sketchybar/sketchybarrc" = {
        executable = true;
        text =
          let
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

            bar' = "\t--bar \\\n\t\t${mkSettings cfg.bar}";
            default' = "\t--default \\\n\t\t${mkSettings cfg.default}";
            events' = map (event: "\t--add event ${event}") cfg.events;
            items' = map mkItem cfg.items;
          in
          ''#!/usr/bin/env dash

          HAS_BATTERY=$(if [ "$(pmset -g batt | grep "Battery")" = "" ]; then echo "false"; else echo "true"; fi)

          ${concatStringsSep " \\\n" (["sketchybar" bar' default'] ++ events' ++ items')}

          sketchybar --update

          echo "sketchybar configuration loaded.."'';
      };
    };

    homebrew = {
      taps = [ "FelixKratz/formulae" ];
      brews = [ "sketchybar" "ifstat" ];
      casks = [ "font-sf-pro" "sf-symbols" ];
    };

    system.defaults.NSGlobalDomain._HIHideMenuBar = true;

    services.yabai.config.external_bar = "all:32:0";

    launchd.user.agents.sketchybar.serviceConfig = {
      ProgramArguments = [ "/usr/local/bin/sketchybar" ];
      EnvironmentVariables.PATH = config.environment.systemPath;
      RunAtLoad = true;
      KeepAlive = true;
      ProcessType = "Interactive";
      Nice = -20;
      StandardOutPath = cfg.logPath;
      StandardErrorPath = cfg.logPath;
    };
  };
}
