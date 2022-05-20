channels: final: prev: {
  __dontExport = true;
  inherit
    (channels.latest)
    alejandra
    cachix
    dhall
    gopls
    go_1_18
    gotools
    gofumpt
    gnupg
    nix
    nixpkgs-fmt
    starship
    ;
}
