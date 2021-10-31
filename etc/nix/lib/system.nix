{ inputs, lib, overlays, files, ... }:

let
  inherit (lib) nixosSystem;
  inherit (files) mapFilesRecToList;
in {
  mkNixosConfig = path:
    nixosSystem {
      system = import (path + "/system.nix");
      specialArgs = { inherit inputs lib; };
      modules = [ { nixpkgs.overlays = [ overlays ]; } (import path) ]
        ++ mapFilesRecToList import ../modules;
    };
}

