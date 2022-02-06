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
        EDITOR = "te";
        EMACSDIR = emacsDir;
        DOOMDIR = doomDir;
        DOOMLOCALDIR = doomDataDir;
      };

      hm.programs.emacs = {
        enable = isLinux;
        package = pkgs.emacsGcc;
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
        nodePackages.markdownlint-cli
        nodePackages.unified-language-server
        nodePackages.prettier
        pandoc

        # :lang nix
        nixpkgs-fmt
        rnix-lsp

        # :lang org
        graphviz

        # :lang go
        gopls
        gotools
        gore
        godef
        gocode
        gomodifytags
        gofumpt
        golangci-lint

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
  }
  (optionalAttrs (hasAttr "launchd" options) {
      launchd.user.agents."emacsd" = {
          command = "/usr/local/bin/emacs --fg-daemon";
          environment = {
              EMACSDIR = emacsDir;
              DOOMDIR = doomDir;
              DOOMLOCALDIR = doomDataDir;
          };
          serviceConfig = {
              RunAtLoad = true;
              StandardOutPath = "${config.my.xdg.cache}/emacsd.out.log";
              StandardErrorPath = "${config.my.xdg.cache}/emacsd.err.log";
          };
      };
  })
  (optionalAttrs (hasAttr "homebrew" options) {
    homebrew = {
      taps = [ "railwaycat/emacsmacport" ];
      brews = [ "pngpaste" ];
      extraConfig = ''
        brew "emacs-mac", args: [
          "with-mac-metal", "with-imagemagick", "with-rsvg",
          "with-natural-title-bar", "with-emacs-big-sur-icon", "with-starter"
        ]'';
    };
  })
]
