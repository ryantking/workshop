{
  config,
  lib,
  ...
}: {
  home.configFile = {
    "sketchybar/sketchybarrc" = {
      executable = true;
      text = ''
        #!/usr/bin/env sh

        source ${config.workshop.configHome}/sketchybar/config.sh'';
    };
  };

  homebrew = {
    taps = ["FelixKratz/formulae"];
    brews = ["sketchybar" "ifstat"];
  };

  system.defaults.NSGlobalDomain._HIHideMenuBar = true;

  services.yabai.config.external_bar = "all:40:0";

  launchd.user.agents.sketchybar.serviceConfig = {
    ProgramArguments = ["/opt/homebrew/bin/sketchybar"];
    EnvironmentVariables = {
      PATH = config.environment.systemPath;
      WORKSHOP_DIR = config.workshop.home;
      XDG_CONFIG_HOME = config.home.configHome;
    };
    RunAtLoad = true;
    KeepAlive = true;
    ProcessType = "Interactive";
    Nice = -20;
    StandardOutPath = "${config.home.cacheHome}/logs/sketchybar.log";
    StandardErrorPath = "${config.home.cacheHome}/logs/sketchybar.log";
  };
}
