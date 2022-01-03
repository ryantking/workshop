{ self, inputs, ... }:

with inputs;
with inputs.nixos;

{
  # nixos = import ./nixos { inherit self inputs; };
  nixpkgs-stable = {
    overlaysBuilder = (channels: [
      (final: prev: {
        inherit (channels.nixpkgs-unstable) nix nix_2_5;
      })
    ]);
  };
  nixpkgs-unstable = { };
  nixpkgs-latest = { };
  nixpkgs-darwin = {
    overlaysBuilder = (channels: [
      (final: prev: {
        inherit (channels.nixpkgs-unstable) direnv nix nix_2_5 nix-direnv;
      })
    ]);
  };
}
