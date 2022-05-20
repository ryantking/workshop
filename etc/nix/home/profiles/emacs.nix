{ self
, config
, options
, lib
, pkgs
, ...
}:
let
  inherit (config) workshop xdg;

  vtermPrintf = pkgs.writeShellScript "vterm_printf" ''
    if [ -n "$TMUX" ] && ([ "''${TERM%%-*}" = "tmux" ] || [ "''${TERM%%-*}" = "screen" ] ); then
        # Tell tmux to pass the escape sequences through
        printf "\ePtmux;\e\e]%s\007\e\\" "$1"
    elif [ "''${TERM%%-*}" = "screen" ]; then
        # GNU screen (screen, screen-256color, screen-256color-bce)
        printf "\eP\e]%s\007\e\\" "$1"
    else
        printf "\e]%s\e\\" "$1"
    fi
  '';

  vtermCmd = pkgs.writeShellScript "vterm_cmd" ''
    vterm_elisp=""
    while [ $# -gt 0 ]; do
        vterm_elisp="$vterm_elisp""$(printf '"%s" ' "$(printf "%s" "$1" | sed -e 's|\\|\\\\|g' -e 's|"|\\"|g')")"
        shift
    done
    ${vtermPrintf} "51;E$vterm_elisp"
  '';

  emacsclient = pkgs.writeShellScriptBin "ec" ''
    if [[  $INSIDE_EMACS == "vterm" ]]; then
       ${vtermCmd} find-file "$(readlink -f "''${@:-.}")"
       exit 0
    fi

    if [[ $TERM == "xterm-kitty" ]]; then
      [[ -f "$HOME/.terminfo/78/xterm-emacs" ]] || \
        /usr/bin/tic -x -o $HOME/.terminfo $WORKSHOP_DIR/etc/terminfo-custom.src
      TERM=xterm-emacs
    fi

    emacsclient -t -a "" $@
  '';

  emacsDir = "${workshop.dataHome}/doom-emacs";
  doomDir = "${workshop.configHome}/doom";
  doomDataDir = "${xdg.dataHome}/doom";
in
{
  shell.env = {
    VISUAL = "ec";
    PATH = [ "${emacsDir}/bin" "$PATH" ];

    DOOMDIR = doomDir;
    DOOMLOCALDIR = doomDataDir;
    LSP_USE_PLISTS = "true";
  };

  xdg = {
    configFile = {
      "emacs" = {
        source = pkgs.sources.chemacs.src;
        recursive = true;
      };

      "chemacs/profiles.el".text = ''
        (("default" . ((user-emacs-directory . "${emacsDir}"))))
      '';

      "chemacs/profile".text = "default";
    };

    dataFile."emacs/site-lisp/mu4e" = {
      source = "${pkgs.mu}/share/emacs/site-lisp/mu4e";
      recursive = true;
    };
  };

  programs.emacs = {
    enable = pkgs.stdenv.isLinux;
    package = pkgs.emacsPgtkGcc;
  };

  home.packages = with pkgs; [
    ## Custom Client Command
    emacsclient

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
