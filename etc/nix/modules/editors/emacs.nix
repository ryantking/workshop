{ pkgs, lib, ... }:

{
  hm.home = {
    packages = [ pkgs.coreutils-prefixed ];

    activation.doomSync = ''
      if [[ -d "$HOME/.config/emacs" ]]; then
        $XDG_CONFIG_HOME/emacs/bin/doom sync
      else
        git clone --depth 1 https://github.com/hlissner/doom-emacs "$HOME/.config/emacs"
        DOOMDIR="$HOME/Workshop/etc/doom" $XDG_CONFIG_HOME/emacs/bin/doom install
      fi
    '';

    sessionPath = [ "$XDG_CONFIG_HOME/emacs/bin" ];

    programs.emacs = {
      enable = pkgs.stdenv.isLinux;
      package = pkgs.emacsGcc;
    };
  };
}
