{
  nix = {
    binaryCaches = [
      "https://ryantking.cachix.org"
      "https://emacs.cachix.org"
      "https://nix-community.cachix.org"
      "https://cache.nixos.org"
    ];
    binaryCachePublicKeys = [
      "ryantking.cachix.org-1:FQS/rxVvhhWSUds7Fcmf4RNdp95gVRHaDxorFuVIgXE="
      "emacs.cachix.org-1:b1SMJNLY/mZF6GxQE+eDBeps7WnkT0Po55TAyzwOxTY="
      "nix-community.cachix.org-1:mB9FSh9qf2dCimDSUo8Zy7bkq5CX+/rkCWyvRCYg3Fs="
      "cache.nixos.org-1:6NCHdD59X431o0gWypbMrAURkbJ16ZPMQFGspcDShjY="
    ];
  };
}
