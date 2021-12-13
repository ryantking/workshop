{ config, pkgs, ... }:

let
  inherit (pkgs) writeShellScriptBin;
  cfg = config.emacs;
in {
  launchd.user.agents = let
    mkEmacsService = command: extra: {
      inherit command;
      environment = {
        EMACSDIR = config.emacs.configDir;
        DOOMDIR = config.emacs.doom.configDir;
        DOOMLOCALDIR = config.emacs.doom.dataDir;
      } // extra;
    };
  in {
    "emacsd" = mkEmacsService "/usr/local/bin/emacs --fg-daemon" {
      serviceConfig.RunAtLoad = true;
    };
    "doom-activate" = mkEmacsService "doom-rebuild" { };
  };

  environment.systemPackages = [
    (writeShellScriptBin "doom-rebuild" ''
      if [[ -d ${config.emacs.configDir} ]]; then
        ${config.emacs.configDir}/bin/doom -y upgrade
      else
        git clone --depth 1 https://github.com/hlissner/doom-emacs ${config.emacs.configDir}
        ${config.emacs.configDir}/bin/doom -y install
      fi

      launchctl setenv EMACSDIR ${config.emacs.configDir}
      launchctl setenv DOOMDIR ${config.emacs.doom.configDir}
      launchctl setenv DOOMLOCALDIR ${config.emacs.doom.dataDir}'')
  ];
}
