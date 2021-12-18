{ config, pkgs, lib, ... }:

let
  inherit (builtins) toString;
  inherit (lib) mkOption types;

  cfg = config.emacs;
in {
  options.emacs = {
    theme = mkOption {
      description = "Emacs theme to apply";
      type = types.str;
    };

    configDir = mkOption {
      description = "Directory to store Emacs config";
      default = "~/.config/emacs";
      type = types.str;
    };

    doom = {
      configDir = mkOption {
        description = "Directory to store Doom config";
        default = "~/.config/doom";
        type = types.str;
      };
      dataDir = mkOption {
        description = "Directory to store Doom data";
        default = "~/.local/share/doom";
        type = types.str;
      };
    };
  };

  config.hm = {
    programs.emacs = {
      enable = pkgs.stdenv.isLinux;
      package = pkgs.emacsGcc;
    };

    home = {
      packages = with pkgs;
        let
          configLoc = "${cfg.doom.configDir}/config.org";
          testConfigLoc =
            "${config.workshopDir}/etc/nix/modules/editors/emacs/config.org";
        in [
          zstd
          (aspellWithDicts (dicts: with dicts; [ en en-computers en-science ]))

          (writeShellScriptBin "emacs-test" ''
            # Replace org file with editable one
            mv ${configLoc} ${configLoc}.bak
            ln -s ${testConfigLoc} ${configLoc}
            doom -y sync

            # Run emacs
            emacs $@

            # Restore original org config
            rm ${configLoc}
            mv ${configLoc}.bak ${configLoc}
            doom -y sync'')
        ];

      sessionPath = [ "$HOME/.config/emacs/bin" ];
      sessionVariables = {
        EMACSDIR = cfg.configDir;
        DOOMDIR = cfg.doom.configDir;
        DOOMLOCALDIR = cfg.doom.dataDir;
      };

      file = {
        ".config/doom/config.org" = {
          source = let
            inherit (builtins) toString;
            inherit (config.identity) name email;
            inherit (config.theme.fonts) mono unicode sans serif;
          in pkgs.substituteAll {
            src = ./config.org;
            gitName = name;
            gitEmail = email;
            theme = cfg.theme;
            monoFamily = mono.family;
            monoWeight = mono.style;
            monoSize = toString mono.size;
            monoBigSize = toString (mono.size * 2);
            unicodeFamily = unicode.family;
            unicodeWeight = unicode.style;
            unicodeSize = toString unicode.size;
            sansFamily = sans.family;
            sansWeight = sans.style;
            sansSize = toString sans.size;
            serifFamily = serif.family;
            serifWeight = serif.style;
            serifSize = toString serif.size;
          };

          onChange = "${cfg.configDir}/bin/doom -y sync";
        };

        ".config/doom/emacs-e.svg".source = ./emacs-e.svg;
        ".config/doom/splash-phrases".source = ./splash-phrases;
      };
    };
  };

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
}
