{ config, lib, options, ... }:

let
  inherit (lib) mkEnableOption mkIf mkMerge mkOption types;

  cfg = config.modules.hardware.video;
in {
  options.modules.hardware.video = {
    gpu = mkOption {
      description = "Type of GPU on the device.";
      type = types.enum [ "nvidia" ];
    };

    hidpi.enable = mkEnableOption "HiDPI support";
  };

  config = mkMerge [
    {
      # hardware.video.hidpi.enable = cfg.hidpi.enable;
    }

    (mkIf (cfg.gpu == "nvidia") {
      hardware = {
        opengl.driSupport32Bit = true;
        # nvidia.modesetting.enable = true;
      };

      services.xserver = {
        videoDrivers = [ "nvidia" ];

        # screenSection = ''
          # Option "metamodes" "nvidia auto-select +0+0 { ForceCompositionPipeline = On }"
        # '';
      };
    })
  ];
}

