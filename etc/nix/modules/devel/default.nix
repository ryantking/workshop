{ pkgs, ... }:

{
  imports = [ ./nix.nix ./go.nix ./rust.nix ];

  config = { hm.home.packages = with pkgs; [ gnumake treefmt ]; };
}
