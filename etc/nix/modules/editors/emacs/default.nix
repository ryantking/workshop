{ config, pkgs, lib, ... }:

let
  emacsDir = "$HOME/.config/emacs";
  doomLocalDir = "$HOME/.local/share/doom";
in {
  hm = {
    programs.emacs = {
      enable = pkgs.stdenv.isLinux;
      package = pkgs.emacsGcc;
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

      activation.ensureDoom = ''
        [[ $(uname) = Darwin ]] && $DRY_RUN_CMD launchctl setenv DOOMLOCALDIR ${doomLocalDir}

        if [[ ! -d ${emacsDir} ]]; then
          $DRY_RUN_CMD git clone --depth 1 https://github.com/hlissner/doom-emacs ${emacsDir}
          $DRY_RUN_CMD ${emacsDir}/bin/doom -y install
        fi
      '';
    };
  };
}

