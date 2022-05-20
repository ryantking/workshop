{ config
, lib
, pkgs
, options
, inputs
, ...
}:
let
  inherit (config.colorscheme) colors;
  font = config.fonts.monospace;
in
{
  programs.kitty = {
    enable = true;
    font.name = "${builtins.replaceStrings [" "] [""] font.family} Nerd Font";

    environment.SSH_PROGRAM = "kitty +kitten ssh";

    keybindings = {
      "ctrl+left" = "neighboring_window left";
      "ctrl+shift+left" = "move_window right";
      "ctrl+down" = "neighboring_window down";
      "ctrl+shift+down" = "move_window up";
      "ctrl+right" = "neighboring_window right";
      "ctrl+shift+right" = "move_window left";
      "ctrl+up" = "neighboring_window up";
      "ctrl+shift+up" = "move_window down";
    };

    settings = {
      font_size = font.size;
      window_padding_width = font.size;
      input_delay = 1;
      tab_bar_style = "slant";
      macos_titlebar_color = "background";
      macos_option_as_alt = true;
      macos_thicken_font = "0.75";
      scrollback_pager = ''${pkgs.neovim}/bin/nvim -u NONE -c "set laststatus=0 clipboard=unnamedplus" -c "autocmd TermOpen * normal G" -c "map q :qa!<CR>" -c "silent write! /tmp/kitty_scrollback_buffer | te echo -n \"$(sed -e 's/\\^[]8;;file:[^\\\\]*\\\\//g' /tmp/kitty_scrollback_buffer)\"; rm -f /tmp/kitty_scrollback_buffer; sleep 1000"'';

      foreground = "#${colors.base04}";
      background = "#${colors.base00}";
      selection_foreground = "#${colors.base04}";
      selection_background = "#${colors.base01}";

      mark1_foreground = "#${colors.base09}";
      mark2_foreground = "#${colors.base09}";
      mark3_foreground = "#${colors.base09}";
      url_color = "#${colors.base05}";
      cursor = "#${colors.base04}";
      cursor_text_color = "#${colors.base03}";
      active_tab_foreground = "#${colors.base05}";
      active_tab_background = "#${colors.base03}";
      inactive_tab_foreground = "#${colors.base04}";
      inactive_tab_background = "#${colors.base01}";
      color0 = "#${colors.base01}";
      color8 = "#${colors.base03}";
      color1 = "#${colors.base08}";
      color9 = "#${colors.base08}";
      color2 = "#${colors.base0B}";
      color10 = "#${colors.base0B}";
      color3 = "#${colors.base0A}";
      color11 = "#${colors.base0A}";
      color4 = "#${colors.base0D}";
      color12 = "#${colors.base0D}";
      color5 = "#${colors.base0E}";
      color13 = "#${colors.base0E}";
      color6 = "#${colors.base0C}";
      color14 = "#${colors.base07}";
      color7 = "#${colors.base05}";
      color15 = "#${colors.base06}";
    };
  };
}
