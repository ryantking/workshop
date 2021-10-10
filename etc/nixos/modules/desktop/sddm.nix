{ config, lib, options, ... }:

let
  inherit (lib) mkEnableOption mkIf;

  cfg = config.modules.desktop.sddm;
in {
  options.modules.desktop.sddm = {
    enable = mkEnableOption "SDDM (requires a desktop manager to be enabled)";
  };

  config = mkIf cfg.enable {
    services.xserver = {
      enable = true;
      displayManager.sddm.enable = true;
    };
  };
}

