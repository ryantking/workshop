{ config, pkgs, lib, ... }:

let inherit (lib) mkOption mkIf mkMerge types;
in {
  options = {
    cpuVendor = mkOption {
      description = "CPU Vendor. Used to enable microcode updates.";
      type = types.nullOr (types.enum [ "intel" ]);
    };

    gpuVendor = mkOption {
      description = "Type of GPU on the device.";
      type = types.enum [ "nvidia" ];
    };
  };

  config = mkMerge [
    {
      sound.enable = true;

      hardware = {
        video.hidpi.enable = true;

        bluetooth = {
          enable = true;
          settings = { General.Enable = "Source,Sink,Media,Socket"; };
        };

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
    (mkIf (config.cpuVendor != null) { hardware.cpu.${config.cpuVendor}.updateMicrocode = true; })
    (mkIf (config.gpuVendor == "nvidia") { services.xserver.videoDrivers = [ "nvidia" ]; })
  ];
}
