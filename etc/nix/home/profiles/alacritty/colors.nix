{ colors
, lib
,
}:
with colors; {
  primary = {
    background = "#${colors.base00}";
    foreground = "#${colors.base04}";
  };

  cursor = {
    text = "#${colors.base00}";
    cursor = "#${colors.base04}";
  };

  vi_mode_cursor = {
    text = "#${colors.base00}";
    cursor = "#${colors.base04}";
  };

  selection = {
    text = "CellBackground";
    background = "#${colors.base03}";
  };

  search = {
    matches = {
      foreground = "CellBackground";
      background = "#${colors.base0C}";
    };
    bar = {
      background = "#${colors.base02}";
      foreground = "#${colors.base04}";
    };
  };

  normal = {
    black = "#${colors.base01}";
    red = "#${colors.base08}";
    green = "#${colors.base0B}";
    yellow = "#${colors.base0A}";
    blue = "#${colors.base0D}";
    magenta = "#${colors.base0E}";
    cyan = "#${colors.base0C}";
    white = "#${colors.base05}";
  };

  bright = {
    black = lib.mkDefault "#${colors.base03}";
    red = lib.mkDefault "#${colors.base08}";
    green = lib.mkDefault "#${colors.base0B}";
    yellow = lib.mkDefault "#${colors.base0A}";
    blue = lib.mkDefault "#${colors.base0D}";
    magenta = lib.mkDefault "#${colors.base0E}";
    cyan = lib.mkDefault "#${colors.base07}";
    white = lib.mkDefault "#${colors.base06}";
  };

  dim = {
    black = lib.mkDefault "#${colors.base01}";
    red = lib.mkDefault "#${colors.base08}";
    green = lib.mkDefault "#${colors.base0B}";
    yellow = lib.mkDefault "#${colors.base0A}";
    blue = lib.mkDefault "#${colors.base0D}";
    magenta = lib.mkDefault "#${colors.base0E}";
    cyan = lib.mkDefault "#${colors.base0C}";
    white = lib.mkDefault "#${colors.base05}";
  };
}
