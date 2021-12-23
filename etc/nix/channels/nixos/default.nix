{ self, inputs, ... }:

with inputs;
with inputs.nixos;

{
  imports = [ (digga.lib.importOverlays ../../overlays/nixos) ];
  overlays = [ nur.overlay agenix.overlay nvfetcher.overlay ];
}
