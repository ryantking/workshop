{ config, pkgs, lib, ... }:

  {
    hm.programs.kitty = with config.theme; {
      enable = true;
      font = {
        name = fonts.mono.family;
        package = fonts.mono.pkg;
      };
      settings = {
        bold_font = "auto";
        italic_font = "auto";
        bold_italic_font = "auto";
        font_size = fonts.mono.size;
        strip_trailing_spaces = "smart";
        enable_audio_bell = "no";
        macos_titlebar_color = "background";
        macos_option_as_alt = "yes";
        mocos_thicken_font = "0.75";
        scrollback_lines = 100000;
        input_delay = 1;
        window_padding_width = 20;

        foreground = colors.fg2;
        background = colors.bg0;
        selection_foreground = colors.fg2;
        selection_background = colors.bg1;
        mark1_foreground = colors.quaternary;
        mark2_foreground = colors.quaternary;
        mark3_foreground = colors.quaternary;
        url_color = colors.fg1;
        cursor = colors.fg2;
        cursor_text_color = colors.bg3;
        active_tab_foreground = colors.fg1;
        active_tab_background = colors.bg3;
        inactive_tab_foreground = colors.fg2;
        inactive_tab_background = colors.bg1;
        color0 = colors.bg1;
        color8 = colors.bg3;
        color1 = colors.alert;
        color9 = colors.alert;
        color2 = colors.secondary;
        color10 = colors.secondary;
        color3 = colors.quinary;
        color11 = colors.quinary;
        color4 = colors.fg3;
        color12 = colors.fg3;
        color5 = colors.tertiary;
        color13 = colors.tertiary;
        color6 = colors.primary;
        color14 = colors.septary;
        color7 = colors.fg1;
        color15 = colors.fg0;
      };
    };
  }
