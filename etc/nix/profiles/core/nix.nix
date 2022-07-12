{
  config,
  lib,
  pkgs,
  ...
}: {
  nix = lib.mkMerge [
    {
      package = pkgs.nix;
      useSandbox = lib.mkDefault true;
      allowedUsers = ["*"];
      trustedUsers = ["root" "@wheel" "rking"];

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
    (lib.optionalAttrs pkgs.stdenv.isLinux {
      autoOptimiseStore = true;
      optimise.automatic = true;
      gc.dates = "weekly";
    })
  ];
}
