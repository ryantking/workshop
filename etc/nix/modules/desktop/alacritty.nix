{ config, ... }:

{
  hm.programs.alacritty = {
    enable = true;
    settings = {
      env.TERM = "xterm-256color";
      mouse.hide_when_typing = true;
      cursor.style = "Beam";
      live_config_reload = true;
      use_thin_strokes = true;

      window = {
        dimension.columns = 120;
        padding = {
          x = 20;
          y = 20;
        };

        decorations = "buttonless";
        opacity = 0.5;
        dynamic_title = true;
      };

      font = with config.theme.fonts.mono; {
        size = size;

        normal = {
          family = family;
          style = style;
        };
      };

      colors = with config.theme.colors; {
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
    };
  };
}
