{ config, lib, pkgs, options, ... }:

let
  inherit (lib) mkEnableOption mkIf;

  cfg = config.modules.hardware.bluetooth;
in {
  options.modules.hardware.bluetooth = {
    enable = mkEnableOption "Bluetooth support";
  };

  config = mkIf cfg.enable {
    hardware = {
      bluetooth = {
        enable = true;
        settings = { General.Enable = "Source,Sink,Media,Socket"; };
      };

      pulseaudio = {
        package = pkgs.pulseaudioFull;
        extraModules = [ pkgs.pulseaudio-modules-bt ];
      };
    };
  };
}

