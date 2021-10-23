{ options, config, lib, ... }:

let
  inherit (builtins) listToAttrs;
  inherit (lib) mkOption nameValuePair types;

  cfg = config.module.theme;

  mkColorOption = name:
    mkOption {
      description = "${name} color of the color palette";
      type = types.str;
    };

  mkFontOption = description:
    mkOption {
      inherit description;
      type = types.submodule {
        options = {
          family = mkOption {
            description = "Font family";
            type = types.str;
          };
          style = mkOption {
            description = "Font style";
            default = "Reglar";
            type = types.str;
          };
          size = mkOption {
            description = "Font size";
            type = types.ints.positive;
          };
          pkg = mkOption {
            description = "Package containing font";
            type = types.package;
          };
        };
      };
    };
in {
  options.modules.theme = {
    colorscheme = mkOption {
      type = types.nullOr types.str;
      default = null;
      description = ''
        Name of the colorscheme to apply to the config.
        The colorscheme must be defined as a nix file in modules/theme/colorr
      '';
    };

    colors = mkOption {
      description =
        "16-color palette for theming apps. Based on the base16 scheme.";
      type = types.submodule {
        options = listToAttrs
          (map (name: nameValuePair name (mkColorOption name)) [
            "bg0"
            "bg1"
            "bg2"
            "bg3"
            "fg0"
            "fg1"
            "fg2"
            "fg3"
            "alert"
            "primary"
            "secondary"
            "tertiary"
            "quaternary"
            "quinary"
            "senary"
            "septary"
          ]);
      };
    };

    fonts = mkOption {
      description = "Fonts to use throughout the system";
      type = types.submodule {
        options = {
          sans = mkFontOption "Sans-serif font";
          serif = mkFontOption "Serif font";
          mono = mkFontOption "Monospace font";
          ui = mkFontOption "Font to use for UI elements";
        };
      };
    };
  };
}
