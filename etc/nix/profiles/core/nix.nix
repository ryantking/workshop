{ self, config, lib, pkgs, ... }:

let
  inherit (lib) mkDefault mkMerge optionalAttrs;
  inherit (pkgs.stdenv) isLinux;
in
{
  nix = mkMerge [
    {
      package = pkgs.nix_2_5;
      useSandbox = mkDefault true;
      allowedUsers = [ "*" ];
      trustedUsers = [ "root" "@wheel" ];

      linkInputs = true;
      generateRegistryFromInputs = true;
      generateNixPathFromInputs = true;

      gc = {
        automatic = true;
        options = "--delete-older-than 3d";
      };

      extraOptions = ''
        min-free = 536870912
        keep-outputs = true
        keep-derivations = true
        fallback = true
      '';
    }
    (optionalAttrs isLinux {
      autoOptimiseStore = true;
      optimise.automatic = true;
      gc.dates = "weekly";

      shellAliases.nixos-option = "nixos-option -I nixpkgs=${self}/lib/compat";
    })
  ];
}
