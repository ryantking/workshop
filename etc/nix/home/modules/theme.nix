{
  inputs,
  config,
  lib,
  pkgs,
  ...
}: let
  inherit (lib) types;
  inherit (pkgs.lib.our) mkOpt;
  inherit (config.colorscheme) colors;

  fontModule = types.submodule {
    options = {
      family = lib.mkOption {type = types.str;};
      pkg = mkOpt (types.nullOr types.package) null;
      style = mkOpt types.str "Regular";
      size = mkOpt types.ints.positive 14;
    };
  };

  optionalFont = font: (lib.optional (! isNull font.pkg) font.pkg);

  cfg = config.fonts;
in {
  options.fonts = {
    serif = mkOpt fontModule {
      family = "IBM Plex Serif";
      pkg = pkgs.ibm-plex;
    };
    sans = mkOpt fontModule {
      family = "IBM Plex Sans";
      pkg = pkgs.ibm-plex;
    };
    monospace = mkOpt fontModule {
      family = "Victor Mono";
      style = "Light";
      pkg = pkgs.victor-mono;
    };
    unicode = mkOpt fontModule {
      family = "Julia Mono";
      pkg = pkgs.julia-mono;
    };
  };

  config = {
    fonts.fontconfig.enable = true;

    home.packages = builtins.concatLists [
      [
        pkgs.fontconfig
        (pkgs.nerdfonts.override {
          fonts = [(builtins.replaceStrings [" "] [""] cfg.monospace.family)];
        })
      ]
      (optionalFont cfg.serif)
      (optionalFont cfg.sans)
      (optionalFont cfg.monospace)
      (optionalFont cfg.unicode)
    ];

    shell.env = {
      WORKSHOP_THEME = lib.mkDefault "none";

      FONT_SANS = cfg.sans.family;
      FONT_SERIF = cfg.serif.family;
      FONT_MONOSPACE = cfg.monospace.family;
      FONT_UNICODE = cfg.unicode.family;
    };

    xdg.configFile."colorrc".text = ''
      # Colorscheme: ${config.colorscheme.name}

      COLORSCHEME="${config.colorscheme.name}"
      COLOR_FG=${colors.base04}
      COLOR_BG=${colors.base00}
      COLOR_FG_ALT=${colors.base06}
      COLOR_BG_ALT=${colors.base03}
      COLOR_BLACK=${colors.base01}
      COLOR_RED=${colors.base08}
      COLOR_GREEN=${colors.base0B}
      COLOR_YELLOW=${colors.base0A}
      COLOR_BLUE=${colors.base0D}
      COLOR_MAGENTA=${colors.base0E}
      COLOR_CYAN=${colors.base0C}
      COLOR_WHITE=${colors.base05}
    '';
  };
}
