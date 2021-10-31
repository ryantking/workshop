{ config, pkgs, lib, ... }:

let
  inherit (lib) mkOption mkIf mkMerge;

  cfg = config.hardware;
in {
  options.hardware = {
    cpu.vendor = mkOption {
      description = "CPU Vendor. Used to enable microcode updates.";
      type = types.nullOr (types.enum [ "intel" ]);
    };

    gpu.vendor = mkOption {
      description = "Type of GPU on the device.";
      type = types.enum [ "nvidia" ];
    };
  };

  config = mkMerge [
    {
      sound.enable = true;

      bluetooth = {
        enable = true;
        settings = { General.Enable = "Source,Sink,Media,Socket"; };
      };

      hardware = {
        video.hidpi.enable = true;

        pulseaudio = with pkgs; {
          enable = true;
          package = pulseaudioFull;
          extraModules = [ pulseaudio-modules-bt ];
          daemon.config.default-sample-rate = 48000;
          configFile = pkgs.runCommand "default.pa" { } ''
            sed 's/module-udev-detect$/module-udev-detect tsched=0/' \
            ${pkgs.pulseaudio}/etc/pulse/default.pa > $out
          '';
        };
      };

      user.extraGroups = [ "audio" "pulse" ];
    }
    (mkIf (cfg.cpu.vendor != null) { hardware.cpu.${cfg.vendor}.updateMicrocode = true; })
    (mkIf (cfg.gpu.vendor == "nvidia") { services.xserver.videoDrivers = [ "nvidia" ]; })
  ];
}
