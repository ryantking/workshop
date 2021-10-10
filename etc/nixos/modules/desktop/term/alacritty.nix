{ config, lib, ... }:

let
  inherit (lib) mapAttrs removePrefix mkEnableOption mkIf;
  inherit (config.modules.theme) colors;

  font = config.modules.theme.fonts.term;

  cfg = config.modules.desktop.term.alacritty;
in {
  options.modules.desktop.term.alacritty = {
    enable = mkEnableOption "Alacritty terminal";
  };

  config = mkIf cfg.enable {
    hm.programs.alacritty.enable = true;

    hm.programs.alacritty.settings = {
      font = {
        size = font.size;
        
        normal.family = font.family;
        italic.family = font.family;
        bold.family = font.family;
      };

      colors = with colors; {
        primary = {
          background = bg0;
          foreground = fg0;
        };

        normal = {
          black = bg0;
          red = alert;
          green = secondary;
          yellow = quinary;
          blue = primary;
          magenta = tertiary;
          cyan = senary;
          white = fg0;
        };

        bright = {
          black = bg2;
          red = alert;
          green = secondary;
          yellow = quinary;
          blue = primary;
          magenta = tertiary;
          cyan = senary;
          white = fg1;
        };
      };

      window = {
        dimension.columns = 120;
      };
    };
  };
}

