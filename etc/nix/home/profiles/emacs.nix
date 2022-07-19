{
  self,
  config,
  options,
  lib,
  pkgs,
  ...
}: let
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

  doom = {
    emacsDir = "${workshop.dataHome}/doom-emacs";
    configDir = "${workshop.configHome}/doom";
    dataDir = "${xdg.dataHome}/emacs/doom";
  };
in {
  shell.env = {
    VISUAL = "ec";
    PATH = ["${doom.emacsDir}/bin" "$PATH"];
    DOOMDIR = doom.configDir;
    DOOMLOCALDIR = doom.dataDir;
    LSP_USE_PLISTS = "true";
  };

  xdg = {
    configFile = {
      "emacs" = {
        source = pkgs.sources.chemacs.src;
        recursive = true;
      };

      "chemacs/profiles.el".text = ''
        (("vanilla" . ((user-emacs-directory . "${workshop.configHome}/emacs")))
        ("doom" . ((user-emacs-directory . "${doom.emacsDir}"))))
      '';

      "chemacs/profile".text = "vanilla";
    };

    dataFile."emacs/site-lisp/mu4e" = {
      source = "${pkgs.mu}/share/emacs/site-lisp/mu4e";
      recursive = true;
    };
  };

  programs.emacs = {
    enable = pkgs.stdenv.isLinux;
    package = pkgs.emacsNativeComp;
  };

  home.packages = with pkgs; [
    emacsclient

    (aspellWithDicts (dicts: with dicts; [en en-computers en-science]))

    imagemagick
    pinentry_emacs
    graphviz
    sqlite
    zstd

    ## Linters
    gopls
    gotools
    golangci-lint
    proselint

    ## LSP
    (python3.withPackages (ps: with ps; [epc python-lsp-server]))
    nodePackages.dockerfile-language-server-nodejs
    nodePackages.vscode-json-languageserver
    nodePackages.bash-language-server
    nodePackages.yaml-language-server
    rnix-lsp
  ];
}
