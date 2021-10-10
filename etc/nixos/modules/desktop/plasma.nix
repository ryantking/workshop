{ config, lib, options, ... }:

let
  inherit (lib) mkEnableOption mkIf;

  cfg = config.modules.desktop.plasma;
in {
  options.modules.desktop.plasma = {
    enable = mkEnableOption "Enable Plasma desktop environment";
  };

  config = mkIf cfg.enable {
    services.xserver = {
      enable = true;
      desktopManager.plasma5.enable = true;
    };
  };
}

