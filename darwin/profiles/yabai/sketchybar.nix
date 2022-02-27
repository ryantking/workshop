{ self, config, ... }:

let
  inherit (config.home) cacheHome;
in {
  homebrew = {
    taps = [ "FelixKratz/formulae" ];
    brews = [ "sketchybar" ];
  };

  system.defaults.NSGlobalDomain._HIHideMenuBar = true;

  services.yabai.config.external_bar = "main:32:0";

  home.configFile."sketchybar/plugins".source = "${self}/etc/sketchybar/plugins";

  # launchd.user.agents."sketchybar".serviceConfig = {
  #   ProgramArguments = [ "/usr/local/bin/sketchybar" ];
  #   RunAtLoad = true;
  #   KeepAlive = true;
  #   ProcessType = "Interactive";
  #   Nice = -20;
  #   StandardOutPath = "${cacheHome}/logs/sketchybar.out.log";
  #   StandardErrorPath = "${cacheHome}/logs/sketchybar.err.log";
  # };
}
