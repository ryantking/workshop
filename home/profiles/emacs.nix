{ self, config, options, lib, pkgs, ... }:

let
  inherit (builtins) hasAttr toString;
  inherit (lib) mkMerge optionalAttrs;
  inherit (config) workshop;
  inherit (config.xdg) configHome dataHome;
  inherit (pkgs.stdenv) isLinux;

  emacsDir = "${configHome}/emacs";
  doomDir = "${workshop.configDir}/doom";
  doomDataDir = "${dataHome}/doom";
in
{
  shell = {
    env = {
      EDITOR = "emacsclient -t";
      VISUAL = "emacsclient -c -a emacs";
      PATH = [ "${emacsDir}/bin" "$PATH" ];

      DOOMDIR = doomDir;
      DOOMLOCALDIR = doomDataDir;
      LSP_USE_PLISTS = "true";
    };

    aliases = {
      ec = "emacsclient -t";
      ecv = "emacsclient";
    };
  };

  xdg = {
    configFile = {
      "emacs" = {
        source = pkgs.sources.chemacs.src;
        recursive = true;
      };

      "chemacs/profiles.el".text = ''
        (("default" . ((user-emacs-directory . "${workshop.configDir}/emacs"))))
      '';

      "chemacs/profile".text = "default";
    };

    dataFile."emacs/site-lisp/mu4e" = {
      source = "${pkgs.mu}/share/emacs/site-lisp/mu4e";
      recursive = true;
    };
  };

  programs.emacs = {
    enable = isLinux;
    package = pkgs.emacsPgtkGcc;
  };

  home.packages = with pkgs; [
    ## Doom Dependencies
    (ripgrep.override { withPCRE2 = true; })
    (python3.withPackages
      (ps: with ps; [ pip black setuptools pylint grip ]))
    gnutls

    ## Fonts
    emacs-all-the-icons-fonts
    julia-mono
    alegreya
    alegreya-sans
    merriweather

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
}
