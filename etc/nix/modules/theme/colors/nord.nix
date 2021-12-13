{ config, lib, pkgs, ... }:

let
  inherit (lib) mkIf;

  cfg = config.theme;

  c-dark0 = "#2e3440";
  c-dark1 = "#3b4252";
  c-dark2 = "#434c5e";
  c-dark3 = "#4c566a";
  c-light0 = "#eceff4";
  c-light1 = "#e5e9f0";
  c-light2 = "#d8dee9";
  c-blue0 = "#8fbcbb";
  c-blue1 = "#88c0d0";
  c-blue2 = "#81a1c1";
  c-blue3 = "#5e81ac";
  c-red = "#bf616a";
  c-orange = "#d08770";
  c-yellow = "#ebcb8b";
  c-green = "#a3be8c";
  c-purple = "#b48ead";
in mkIf (cfg.colorscheme == "nord") {
  theme.colors = {
    bg0 = c-dark0;
    bg1 = c-dark1;
    bg2 = c-dark2;
    bg3 = c-dark3;
    fg0 = c-light0;
    fg1 = c-light1;
    fg2 = c-light2;
    fg3 = c-blue2;
    alert = c-red;
    primary = c-blue1;
    secondary = c-green;
    tertiary = c-purple;
    quaternary = c-orange;
    quinary = c-yellow;
    senary = c-blue3;
    septary = c-blue0;
  };

  hm = {
    home.file.".config/emacs/generated/+theme.el".text = "(setq doom-theme 'doom-nord)";

    programs = {
      bat.config.theme = "Nord";
      git.delta.options.syntax-theme = "Nord";

      neovim = {
        plugins = [ pkgs.vimPlugins.nord-nvim ];
        extraConfig = ''
          let g:nord_borders = v:true
          color nord
          hi IndentBlanklineContextChar guifg=${c-purple}
          hi VertSplit guifg=${c-blue1}
        '';
      };
    };
  };
}
