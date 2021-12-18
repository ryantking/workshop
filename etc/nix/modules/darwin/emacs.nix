{ config, pkgs, lib, ... }:

let
  inherit (builtins) isString;
  inherit (lib) mkIf mkMerge;

  cfg = config.emacs;
in {
  launchd.user.agents = let
    mkEmacsService = { name, command ? null, script ? null, pkgs ? [ ] }:
      mkMerge [
        {
          path = pkgs;
          environment = {
            EMACSDIR = cfg.configDir;
            DOOMDIR = cfg.doom.configDir;
            DOOMLOCALDIR = cfg.doom.dataDir;
          };
          serviceConfig = {
            RunAtLoad = true;
            StandardOutPath = "/tmp/${name}.stdout.log";
            StandardErrorPath = "/tmp/${name}.stderr.log";
          };
        }
        (mkIf (isString command) { inherit command; })
        (mkIf (isString script) { inherit script; })
      ];
  in {
    "emacsd" = mkEmacsService {
      name = "emacsd";
      command = "/usr/local/bin/emacs --fg-daemon";
    };
    "doom-activate" = mkEmacsService {
      name = "doom-activate";
      pkgs = with pkgs; [ "/bin" "/usr/local/bin" git ];
      script = ''
        echo $PATH
        if [[ -d ${cfg.configDir} ]]; then
          ${cfg.configDir}/bin/doom -y upgrade
        else
          git clone --depth 1 https://github.com/hlissner/doom-emacs ${cfg.configDir}
          ${cfg.configDir}/bin/doom -y install
        fi

        launchctl setenv EMACSDIR ${cfg.configDir}
        launchctl setenv DOOMDIR ${cfg.doom.configDir}
        launchctl setenv DOOMLOCALDIR ${cfg.doom.dataDir}'';
    };
  };
}
