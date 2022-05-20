{ config
, lib
, pkgs
, ...
}:
let
  inherit (config) workshop xdg;
in
{
  imports = [ ./shell ];

  shell.env = {
    ZSH_CACHE = "${xdg.cacheHome}/zsh";
    ZSH_DATA = "${xdg.dataHome}/zsh";
    ZINIT_HOME = "${workshop.dataHome}/zinit";
  };

  home.packages = [ pkgs.zoxide ];

  programs.zsh = {
    enable = true;
    dotDir = ".config/zsh";
    initExtra = ''
      source "${workshop.configHome}/zsh/plugins.zsh"
      source "${workshop.configHome}/zsh/config.zsh"

      ${config.shell.rcInit}

      typeset -aU path
    '';
  };

  home.activation.runZinitUpdate = lib.hm.dag.entryAfter [ "writeBoundary" ] ''
    ZINIT_HOME="${workshop.configHome}/zinit"
    ${pkgs.zsh}/bin/zsh -c "source "$ZINIT_HOME/zinit.zsh" && zinit self-update >/dev/null"
  '';
}
