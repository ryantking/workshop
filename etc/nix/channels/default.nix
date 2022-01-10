{ self, inputs, ... }:

with inputs;

{
  nixpkgs-stable = {
    overlaysBuilder =
      (channels: [ (final: prev: { inherit (channels.nixpkgs-unstable) nix nix_2_5; }) ]);
  };
  nixpkgs-unstable = { };
  nixpkgs-latest = { };
  nixpkgs-darwin = {
    overlaysBuilder = (channels: [
      (import ./pkgs/darwin { inherit self inputs; })
      (final: prev: { inherit (channels.nixpkgs-unstable) direnv nix nix_2_5 nix-direnv; })
      spacebar.overlay
    ]);
  };
}
