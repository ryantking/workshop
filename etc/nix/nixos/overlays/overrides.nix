channels: final: prev: {
  __dontExport = true;

  inherit (channels.latest)
    cachix dhall discord element-desktop rage nixpkgs-fmt signal-desktop starship;
}
