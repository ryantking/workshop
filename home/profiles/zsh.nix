{ self, config, lib, pkgs, ... }:

let
  inherit (lib) optionals;
  inherit (pkgs.stdenv) isLinux;
  inherit (lib) concatStrings;

  zdotDir = "${self}/etc/zsh";
in
{
  imports = [ ./shell ];

  shell.env = {
    ZSH_CACHE = "${config.xdg.cacheHome}/zsh";
    ZSH_DATA = "${config.xdg.dataHome}/zsh";
  };

  home.packages = with pkgs; [ zinit zoxide ];

  programs.zsh = {
    enable = true;
    dotDir = ".config/zsh";
    initExtra = ''
      source "''${ZDOTDIR}/config.zsh"
      source "''${ZDOTDIR}/plugins.zsh"
      source "''${ZDOTDIR}/extra.zsh"
      typeset -aU path
    '';
  };

  xdg.configFile = {
    "zsh/config.zsh".source = "${zdotDir}/config.zsh";

    "zsh/plugins.zsh" = {
      source = "${zdotDir}/plugins.zsh";
      onChange = "fd -uu --extension zwc --exec rm '{}'";
    };

    "zsh/extra.zsh".text = config.shell.rcInit;

    "zsh/zinit.zsh".text = ''
      . ${pkgs.zinit}/share/zinit/zinit.zsh
    '';
  };
}
