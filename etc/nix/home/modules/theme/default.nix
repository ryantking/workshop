{ config, lib, pkgs, ... }:

with lib.types;

let
  inherit (lib.our) mkOpt;

  cfg = config.theme;
in {
  imports = [ ./colors/nord.nix ];

  options.theme = {
    colorscheme = mkOpt str "nord";

    codeFont = {
      family = mkOpt str "Victor Mono";
      style = mkOpt str "Regular";
      size = mkOpt ints.positive 10;
      pkg = mkOpt package pkgs.victor-mono;
      nerdFamily = mkOpt str "VictorMono";
    };
  };

  config = {
    fonts.fontconfig.enable = true;

    home.packages =
      [ cfg.codeFont.pkg (pkgs.nerdfonts.override { fonts = [ cfg.codeFont.nerdFamily ]; }) ];
  };
}
