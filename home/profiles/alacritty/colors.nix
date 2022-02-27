{ colors, lib }:

with colors;

{
  primary = {
    background = "#${base00}";
    foreground = "#${base04}";
  };

  cursor = {
    text = "#${base00}";
    cursor = "#${base04}";
  };

  vi_mode_cursor = {
    text = "#${base00}";
    cursor = "#${base04}";
  };

  selection = {
    text = "CellBackground";
    background = "#${base03}";
  };

  search = {
    matches = {
      foreground = "CellBackground";
      background = "#${base0C}";
    };
    bar = {
      background = "#${base02}";
      foreground = "#${base04}";
    };
  };

  normal = {
    black = "#${base01}";
    red = "#${base08}";
    green = "#${base0B}";
    yellow = "#${base0A}";
    blue = "#${base0D}";
    magenta = "#${base0E}";
    cyan = "#${base0C}";
    white = "#${base05}";
  };

  bright = {
    black = "#${base03}";
    red = "#${base08}";
    green = "#${base0B}";
    yellow = "#${base0A}";
    blue = "#${base0D}";
    magenta = "#${base0E}";
    cyan = "#${base07}";
    white = "#${base06}";
  };

  dim = {
    black = lib.mkDefault "#${base01}";
    red = lib.mkDefault "#${base08}";
    green = lib.mkDefault "#${base0B}";
    yellow = lib.mkDefault "#${base0A}";
    blue = lib.mkDefault "#${base0D}";
    magenta = lib.mkDefault "#${base0E}";
    cyan = lib.mkDefault "#${base0C}";
    white = lib.mkDefault "#${base05}";
  };
}
