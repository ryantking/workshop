{ config, lib, pkgs, inputs, ... }:

with inputs;

lib.mkIf (config.colorscheme.slug == "nord") {
  # emacs.theme = "doom-nord";

  my.hm.programs = {
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

    neovim = {
      plugins = [ pkgs.vimPlugins.nord-nvim ];
      extraConfig =
        let inherit (config.colorscheme) colors;
        in
        ''
          let g:nord_borders = v:true
          color nord
          hi IndentBlanklineContextChar guifg=${colors.base0E}
          hi VertSplit guifg=${colors.base8}
        '';
    };
  };
}
