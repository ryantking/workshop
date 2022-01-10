{ config, lib, pkgs, ... }:

with lib.types;

let
  inherit (pkgs.lib.our) mkOpt;

  cfg = config.theme;
in
{
  imports = [ ./colors/nord.nix ];

  options.theme = {
    codeFont = {
      family = mkOpt str "Victor Mono";
      style = mkOpt str "Regular";
      size = mkOpt ints.positive 12;
      pkg = mkOpt package pkgs.victor-mono;
      nerdFamily = mkOpt str "VictorMono";
    };
  };
}
