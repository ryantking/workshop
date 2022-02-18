{ options, lib, pkgs, ... }:


let
  inherit (builtins) hasAttr;
  inherit (lib) mkMerge optionals optionalAttrs;
  inherit (pkgs.stdenv) isLinux;
in
mkMerge [
  {
    environment.systemPackages = with pkgs; [
      go_1_18
      (writeScriptBin "go1.18" "${pkgs.go_1_18}/bin/go $@")
      (writeScriptBin "gofmt1.18" "${pkgs.go_1_18}/bin/gofmt $@")
    ] ++ (optionals isLinux [ go_1_17 ]);
  }
  (optionalAttrs (hasAttr "homebrew" options) {
    homebrew.brews = [ "go" ];
  })
]
