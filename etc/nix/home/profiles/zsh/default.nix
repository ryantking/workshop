{ config, lib, pkgs, ... }:

let
  inherit (lib) optionals;
  inherit (pkgs.stdenv) isLinux;
in
{
  imports = [ ../shell ];

  my = {
    user.packages = with pkgs; [ zsh zinit zoxide ];

    env = rec {
      ZDOTDIR = "$XDG_CONFIG_HOME/zsh";
      ZSH_CACHE = "$XDG_CACHE_HOME/zsh";
      ZSH_DATA = "$XDG_DATA_HOME/zsh";
    };

    hm = {
      programs.starship = { enableZshIntegration = false; };

      xdg = {
        configFile = {
          "zsh" = {
            source = "${config.workshop.configDir}/zsh";
            recursive = true;
            onChange = "fd -uu --extension zwc --exec rm '{}'";
          };

          "zsh/.zshenv".text = ''
            [ -f ~/.zshenv ] && source ~/.zshenv
          '';

          "zsh/extra.zsh".text = config.my.shell.rcInit;
          "zsh/zinit.zsh".text = ''
            . ${pkgs.zinit}/share/zinit/zinit.zsh
          '';
        };
      };
    };
  };
}
