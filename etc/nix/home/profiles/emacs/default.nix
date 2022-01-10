{ inputs, config, options, lib, pkgs, ... }:

let
  inherit (builtins) hasAttr toString;
  inherit (config) home-manager my;
  inherit (lib) mkMerge optionalAttrs;
  inherit (home-manager.users.${my.username}.xdg) configHome dataHome;
  inherit (pkgs.stdenv) isLinux;

  emacsDir = "${configHome}/emacs";
  doomDir = "${config.workshop.path}/etc/doom";
  doomDataDir = "${dataHome}/doom";
in
mkMerge [
  {
    environment.variables = { PATH = [ "${emacsDir}/bin" "$PATH" ]; };

    fonts.fonts = with pkgs; [ emacs-all-the-icons-fonts julia-mono alegreya alegreya-sans merriweather ];

    my = {
      env = {
        EDITOR = "emacsclient";
        EMACSDIR = emacsDir;
        DOOMDIR = doomDir;
        DOOMLOCALDIR = doomDataDir;
      };

      hm = {
        programs.emacs = {
          enable = isLinux;
          package = pkgs.emacsGcc;
        };

        # xdg.configFile."emacs" = {
        # source = inputs.doom-emacs-source;
        # force = true;
        # };
      };

      user.packages = with pkgs; [
        ## Doom Dependencies
        (ripgrep.override { withPCRE2 = true; })
        (python3.withPackages
          (ps: with ps; [ pip black setuptools pylint grip ]))
        gnutls

        ## Optional Dependencies
        fd
        imagemagick
        pinentry_emacs
        zstd

        ## Module dependencies
        # :checkers spell
        (aspellWithDicts (ds: with ds; [ en en-computers en-science ]))

        # :checkers grammar
        languagetool

        # :term vterm
        cmake

        # :tools docker
        nodePackages.dockerfile-language-server-nodejs

        # :tools lookup
        # & :lang org +roam
        sqlite

        # :lang json
        nodePackages.vscode-json-languageserver

        # :lang markdown
        nodePackages.unified-language-server

        # :lang nix
        nixpkgs-fmt
        rnix-lsp

        # :lang org
        graphviz

        # :lang python
        python39Packages.python-lsp-server

        # :lang sh
        nodePackages.bash-language-server

        # :lang toml
        taplo-lsp

        # :lang yaml
        nodePackages.yaml-language-server
      ];
    };

    # home = {
    #   packages = with pkgs;
    #     let
    #       configLoc = "${cfg.doom.configDir}/config.org";
    #       testConfigLoc =
    #         "${config.workshopDir}/etc/nix/modules/editors/emacs/config.org";
    #     in [

    #       (writeShellScriptBin "emacs-test" ''
    #         # Replace org file with editable one
    #         mv ${configLoc} ${configLoc}.bak
    #         ln -s ${testConfigLoc} ${configLoc}
    #         doom -y sync

    #         # Run emacs
    #         emacs $@

    #         # Restore original org config
    #         rm ${configLoc}
    #         mv ${configLoc}.bak ${configLoc}
    #         doom -y sync'')
    #     ];

    #   sessionPath = [ "$HOME/.config/emacs/bin" ];
    #   sessionVariables = {
    #     EMACSDIR = cfg.configDir;
    #     DOOMDIR = cfg.doom.configDir;
    #     DOOMLOCALDIR = cfg.doom.dataDir;
    #   };

    #   file = {
    #     ".config/doom/config.org" = {
    #       source = let
    #         inherit (builtins) toString;
    #         inherit (config.identity) name email;
    #         inherit (theme.fonts) mono unicode sans serif;
    #       in pkgs.substituteAll {
    #         src = ./config.org;
    #         gitName = name;
    #         gitEmail = email;
    #         theme = cfg.theme;
    #         monoFamily = mono.family;
    #         monoWeight = mono.style;
    #         monoSize = toString mono.size;
    #         monoBigSize = toString (mono.size * 2);
    #         unicodeFamily = unicode.family;
    #         unicodeWeight = unicode.style;
    #         unicodeSize = toString unicode.size;
    #         sansFamily = sans.family;
    #         sansWeight = sans.style;
    #         sansSize = toString sans.size;
    #         serifFamily = serif.family;
    #         serifWeight = serif.style;
    #         serifSize = toString serif.size;
    #       };

    #       # onChange = "${cfg.configDir}/bin/doom -y sync";
    #     };

    #     ".config/doom/emacs-e.svg".source = ./emacs-e.svg;
    #     ".config/doom/splash-phrases".source = ./splash-phrases;
    #     # ".config/emacs" = {
    #     #   source = inputs.doom-emacs-source;
    #     #   force = true;
    #     # };
    #   };
    # };

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
  }
  (optionalAttrs (hasAttr "homebrew" options) {
    homebrew = {
      taps = [ "railwaycat/emacsmacport" ];
      brews = [ "pngpaste" ];
      extraConfig = ''
        brew "emacs-mac", args: ["with-emacs-big-sur-icon", "with-imagemagick", "with-mac-metal", "with-natural-title-bar", "with-rsvg", "with-starter"]'';
    };
  })
]
