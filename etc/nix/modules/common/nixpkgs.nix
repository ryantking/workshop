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

    gc = {
      automatic = true;
      options = "--delete-older-than 30d";
      user = "rking";
    };

    binaryCaches =
      [ "https://ryantking.cachix.org" "https://nix-community.cachix.org" ];

    binaryCachePublicKeys = [
      "ryantking.cachix.org-1:FQS/rxVvhhWSUds7Fcmf4RNdp95gVRHaDxorFuVIgXE="
      "nix-community.cachix.org-1:mB9FSh9qf2dCimDSUo8Zy7bkq5CX+/rkCWyvRCYg3Fs="
    ];
  };
}
