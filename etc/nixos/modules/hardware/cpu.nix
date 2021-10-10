{ config, lib, options, ... }:

let
  inherit (lib) mkEnableOption mkIf mkMerge mkOption types;

  cfg = config.modules.hardware.cpu;
in {
  options.modules.hardware.cpu = {
    vendor = mkOption {
      description = "CPU Vendor. Used to enable microcode updates.";
      type = types.nullOr (types.enum [ "intel" ]);
    };
  };

  config = mkIf (cfg.vendor != null) {
    hardware.cpu.${cfg.vendor}.updateMicrocode = true;
  };
}

