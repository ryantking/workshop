{ config, pkgs, ... }:

let
  inherit (config) emacs;
  inherit (config.home) configHome dataHome cacheHome;
in
{
  homebrew = {
    taps = [ "railwaycat/emacsmacport" ];
    brews = [ "pngpaste" ];
    extraConfig = ''
      brew "emacs-mac", args: [
        "with-mac-metal", "with-imagemagick", "with-rsvg",
        "with-natural-title-bar", "with-emacs-big-sur-icon", "with-starter"
      ]'';
  };

  launchd.user.agents."emacsd" = {
    command = "/usr/local/bin/emacs --fg-daemon";

    environment = {
      EMACSDIR = emacs.configDir;
      DOOMDIR = emacs.doom.configDir;
      DOOMLOCALDIR = emacs.doom.dataDir;
    };

    serviceConfig = {
      RunAtLoad = true;
      StandardOutPath = "${cacheHome}/logs/emacsd.out.log";
      StandardErrorPath = "${cacheHome}/logs/emacsd.err.log";
    };
  };
}
