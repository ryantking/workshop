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
  };
}
