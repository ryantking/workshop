{
  config,
  lib,
  pkgs,
  ...
}: {
  hardware.bluetooth = {
    enable = true;
    package = pkgs.bluezFull;
  };

  # hardware.bluetooth = {
  #   enable = true;
  #   settings = {General.Enable = "Source,Sink,Media,Socket";};
  # };
}
