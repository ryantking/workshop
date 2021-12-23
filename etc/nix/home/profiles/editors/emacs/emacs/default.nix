{ inputs, config, pkgs, lib, theme, ... }:

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
      default = "${config.xdg.configHome}/emacs";
      type = types.str;
    };

    doom = {
      configDir = mkOption {
        description = "Directory to store Doom config";
        default = "${config.xdg.configHome}/doom";
        type = types.str;
      };

      dataDir = mkOption {
        description = "Directory to store Doom data";
        default = "${config.xdg.dataHome}/doom";
        type = types.str;
      };
    };
  };

  config = {
    programs.emacs = {
      enable = pkgs.stdenv.isLinux;
      package = pkgs.emacsGcc;
    };

    home = {
      packages = with pkgs;
        let
          configLoc = "${cfg.doom.configDir}/config.org";
          testConfigLoc = "${config.workshopDir}/etc/nix/modules/editors/emacs/config.org";
        in [
          doom-src
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
            inherit (theme.fonts) mono unicode sans serif;
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

          # onChange = "${cfg.configDir}/bin/doom -y sync";
        };

        ".config/doom/emacs-e.svg".source = ./emacs-e.svg;
        ".config/doom/splash-phrases".source = ./splash-phrases;
        # ".config/emacs" = {
        #   source = inputs.doom-emacs-source;
        #   force = true;
        # };
      };
    };

    # systemd.user.services.doom-rebuild = {
    #   Unit = {
    #     Description = "Doom Emacs Manager";
    #     After = [ "network.online.target" ];
    #   };
    #   Install = { WantedBy = [ "default.target" ]; };
    #   Service = with pkgs; {
    #     Type = "simple";
    #     Environment = [
    #       "PATH=${bash}/bin:${git}/bin:${coreutils}/bin:${emacsPgtkGcc}/bin:${cfg.configDir}/bin:$PATH"
    #       "EMACSDIR=${cfg.configDir}"
    #       "DOOMDIR=${cfg.doom.configDir}"
    #       "DOOMLOCALDIR=${cfg.doom.dataDir}"
    #     ];
    #     ExecStart = toString (writeShellScript "doom-rebuild" ''
    #       set -euo pipefail
    #
    #       # if [[ -d ${cfg.doom.dataDir} ]]; then
    #       #   doom -y sync -u
    #       #   exit 0
    #       # fi
    #
    #       echo "installing"
    #       doom -y install
    #       sed -i 's/;;literate/literate/' ${cfg.doom.configDir}/init.el
    #       doom -y sync 
    #     '');
    #   };
    # };
  };
}
