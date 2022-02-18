{ inputs, common }:

with inputs;

{
  nixpkgs-darwin = {
    imports = common.imports ++ [(digga.lib.importOverlays ./overlays)];
    overlays = common.overlays ++ [ ./pkgs ];
  };
}
