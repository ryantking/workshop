{ config, lib, pkgs, options, inputs, ... }:

with config.colorscheme.colors;

let
  inherit (builtins) hasAttr;
  inherit (config) theme;
  inherit (lib) mkMerge optionalAttrs;
  inherit (pkgs.stdenv) isDarwin;
in
mkMerge [
  {
    programs.kitty = {
      enable = true;
      font = {
        name = "VictorMono Nerd Font Mono";
        package = pkgs.nerdfonts.override { fonts = [ "VictorMono" ]; };
      };
      settings = {
        font_size = 12;
        window_padding_width = 12;
        input_delay = 1;

        foreground = "#${base04}";
        background = "#${base00}";
        selection_foreground = "#${base04}";
        selection_background = "#${base01}";
        mark1_foreground = "#${base09}";
        mark2_foreground = "#${base09}";
        mark3_foreground = "#${base09}";
        url_color = "#${base05}";
        cursor = "#${base04}";
        cursor_text_color = "#${base03}";
        active_tab_foreground = "#${base05}";
        active_tab_background = "#${base03}";
        inactive_tab_foreground = "#${base04}";
        inactive_tab_background = "#${base01}";
        color0 = "#${base01}";
        color8 = "#${base03}";
        color1 = "#${base08}";
        color9 = "#${base08}";
        color2 = "#${base0B}";
        color10 = "#${base0B}";
        color3 = "#${base0A}";
        color11 = "#${base0A}";
        color4 = "#${base0D}";
        color12 = "#${base0D}";
        color5 = "#${base0E}";
        color13 = "#${base0E}";
        color6 = "#${base0C}";
        color14 = "#${base07}";
        color7 = "#${base05}";
        color15 = "#${base06}";
      };
    };
  }

  (optionalAttrs (hasAttr "homebrew" options) { homebrew.casks = [ "kitty" ]; })
]

