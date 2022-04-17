{ config, lib, pkgs, inputs, ... }:

with inputs;

lib.mkIf (config.colorscheme.slug == "nord") {
  shell.env.EMACS_THEME = "doom-nord";

  programs = {
    bat.config.theme = "Nord";
    git.delta.options.syntax-theme = "Nord";

    alacritty.settings.colors.dim = {
      black = "#373e4d";
      red = "#94545d";
      green = "#809575";
      yellow = "#b29e75";
      blue = "#68809a";
      magenta = "#8c738c";
      cyan = "#6d96a5";
      white = "#aeb3bb";
    };
  };
}
