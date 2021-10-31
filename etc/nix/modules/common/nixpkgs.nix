{ config, pkgs, ... }:

{
  nixpkgs.config = {
    allowUnsupportedSystem = true;
    allowUnfree = true;
    allowBroken = false;
  };

  nix = {
    package = pkgs.nixUnstable;
    extraOptions = ''
      experimental-features = nix-command flakes
    '';

    trustedUsers = [ "${config.user.name}" "root" "@admin" "@wheel" ];
    readOnlyStore = true;

    gc = {
      automatic = true;
      options = "--delete-older-than 30d";
      user = "${config.user.name}";
    };

    nixPath = [
      "nixpkgs=/etc/${config.environment.etc.nixpkgs.target}"
      "home-manager=/etc/${config.environment.etc.home-manager.target}"
    ];

    binaryCaches = [
      "https://ryantking.cachix.org"
      "https://nix-community.cachix.org"
      "https://cache.nixos.org"
    ];

    binaryCachePublicKeys = [
      "ryantking.cachix.org-1:FQS/rxVvhhWSUds7Fcmf4RNdp95gVRHaDxorFuVIgXE="
      "nix-community.cachix.org-1:mB9FSh9qf2dCimDSUo8Zy7bkq5CX+/rkCWyvRCYg3Fs="
      "cache.nixos.org-1:6NCHdD59X431o0gWypbMrAURkbJ16ZPMQFGspcDShjY="
    ];
  };
}
