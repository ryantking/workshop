{
  nix = {
    binaryCaches = [
      "https://ryantking.cachix.org"
      "https://nix-community.cachix.org"
      "https://cache.nixos.org"
    ];
    binaryCachePublicKeys = [
      "ryantking.cachix.org-1:FQS/rxVvhhWSUds7Fcmf4RNdp95gVRHaDxorFuVIgXE="
      "nix-community.cachix.org-1:mB9FSh9qf2dCimDSUo8Zy7bkq5CX+/rkCWyvRCYg3Fs="
    ];
  };
}
