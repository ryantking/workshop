{
  inputs,
  config,
  lib,
  pkgs,
  ...
}: {
  colorscheme = inputs.colors.colorSchemes.gruvbox-dark-soft;

  shell.env.EMACS_THEME = "doom-gruvbox";

  programs = {
    bat.config.theme = "Gruvbox";
    git.delta.options.syntax-theme = "Gruvbox";

    alacritty.settings.colors.bright = {
      black = "#928374";
      red = "#fb4934";
      green = "#b8bb26";
      yellow = "#fabd2f";
      blue = "#83a598";
      magenta = "#d3869b";
      cyan = "#8ec07c";
      white = "#ebdbb2";
    };
  };
}
