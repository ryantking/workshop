{ inputs, config, lib, pkgs, ... }:

with inputs;
with lib.types;

let
  inherit (builtins) replaceStrings;
  inherit (pkgs.lib.our) mkOpt mkNerdFamily;

  cfg = config.theme;
in
{
  imports = [ ./colors/nord.nix ];

  options.theme = {
    colorscheme = mkOpt str "nord";

    codeFont = {
      family = mkOpt str "Victor Mono";
      style = mkOpt str "Regular";
      size = mkOpt ints.positive 12;
      pkg = mkOpt package pkgs.victor-mono;
    };
  };

  config = {
    colorscheme = colors.colorSchemes.${cfg.colorscheme};

    fonts.fontconfig.enable = true;

    home.packages = with pkgs;
      [ fontconfig cfg.codeFont.pkg (nerdfonts.override { fonts = [ (mkNerdFamily cfg.codeFont.family) ]; }) ];

    xdg.configFile."colorrc".text = with config.colorscheme.colors; ''
      # Colorscheme: ${config.colorscheme.name}

      COLORSCHEME=${config.colorscheme.name}
      COLOR_FG=${base04}
      COLOR_BG=${base00}
      COLOR_FG_ALT=${base06}
      COLOR_BG_ALT=${base03}
      COLOR_BLACK=${base01}
      COLOR_RED=${base08}
      COLOR_GREEN=${base0B}
      COLOR_YELLOW=${base0A}
      COLOR_BLUE=${base0D}
      COLOR_MAGENTA=${base0E}
      COLOR_CYAN=${base0C}
      COLOR_WHITE=${base05}
    '';
  };
}
