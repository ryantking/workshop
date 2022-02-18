{ inputs, common }:

with inputs;

{
  nixpkgs = {
    imports = common.imports;
    overlays = common.overlays;
  };

  nixpkgs-latest = { };
}
