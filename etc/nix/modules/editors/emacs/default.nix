{ config, pkgs, lib, ... }:

let
  inherit (lib) mkMerge mkIf;
  inherit (pkgs) writeShellScript;
  inherit (pkgs.stdenv) isLinux;

  emacsDir = "$HOME/.config/emacs";
  doomLocalDir = "$HOME/.local/share/doom";
in {
  hm = mkMerge [
    {
      programs.emacs = {
        enable = pkgs.stdenv.isLinux;
        package = pkgs.emacsPgtkGcc;
      };

      home = {
        packages = with pkgs;
          let
            darwinAr = writeShellScriptBin "ar" "/usr/bin/ar $@";
            emacsTest = writeShellScriptBin "emacstest"
              "DOOMDIR=$HOME/Workshop/etc/nix/modules/editors/emacs/doom emacs $@";
          in [ emacsTest ];

        sessionPath = [ "${emacsDir}/bin" ];
        sessionVariables.DOOMLOCALDIR = doomLocalDir;

        file.".config/doom" = {
          source = ./doom;
          onChange = "$DRY_RUN_CMD ${emacsDir}/bin/doom -y sync -u";
        };

        # activation.ensureDoom = ''
        #   [[ $(uname) = Darwin ]] && $DRY_RUN_CMD launchctl setenv DOOMLOCALDIR ${doomLocalDir}
        # '';
      };
    }
    # (mkIf isLinux {
    #   systemd.user.services.update-doom = {
    #     Unit = {
    #       Description = "Doom Emacs Manager";
    #       After = [ "network.online.target" ];
    #     };
    #     Install = { WantedBy = [ "default.target" ]; };
    #     Service = with pkgs; {
    #       Type = "oneshot";
    #       Environment = [
    #         "PATH=${emacsPgtkGcc}/bin:${bash}/bin:${git}/bin:${emacsDir}/bin:$PATH"
    #         "DOOMLOCALDIR=${doomLocalDir}" 
    #       ];
    #       ExecStart = toString (writeShellScript "ensure-doom" ''
    #         set -euo pipefail
    #         echo $PATH
    #
    #         if [[ -d ${emacsDir} ]]; then
    #           ${emacsDir}/bin/doom -y upgrade
    #         else
    #           git clone --depth 1 https://github.com/hlissner/doom-emacs ${emacsDir}
    #           ${emacsDir}/bin/doom -y install
    #         fi
    #       '');
    #     };
    #   };
    # })
  ];
}

