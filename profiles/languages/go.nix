{ options, lib, pkgs, ... }:


let
  inherit (builtins) hasAttr;
  inherit (lib) mkMerge optionals optionalAttrs;
  inherit (pkgs.stdenv) isLinux;
in
mkMerge [
  {
    environment.systemPackages = with pkgs; (optionals isLinux [ go_1_17 ]);
  }
  (optionalAttrs (hasAttr "homebrew" options) {
    homebrew.brews = [ "go" ];
  })
]
