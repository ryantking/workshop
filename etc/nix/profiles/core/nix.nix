{ config, lib, pkgs, ... }:

{
  nix = {
    package = pkgs.nixUnstable;
    generateRegistryFromInputs = lib.mkDefault true;
    # autoOptimiseStore = true;
    # optimise.automatic = true;
    useSandbox = true;
    readOnlyStore = false;
    allowedUsers = [ "@wheel" ];
    trustedUsers = [ "root" "@wheel" ];

    # localRegistry = {
    #   enable = true;
    #   cacheGlobalRegsitry = true;
    #   noGlobalRegistry = false;
    # };

    gc = {
      automatic = true;
      # dates = "weekly";
      options = "--delete-older-than 3d";
    };

    extraOptions = ''
      min-free = 536870912
      keep-outputs = true
      keep-derivations = true
      fallback = true
    '';
  };
}
