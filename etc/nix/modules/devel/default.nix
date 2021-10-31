{ pkgs, ... }:

{
  imports = [ ./nix.nix ./go.nix ];

  config = { hm.home.packages = with pkgs; [ gnumake ]; };
}
