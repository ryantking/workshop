{ config, lib, options, ... }:

let
  inherit (lib) mkEnableOption mkIf;

  cfg = config.modules.hardware.audio;
in {
  options.modules.hardware.audio = {
    enable = mkEnableOption "audio support";
  };

  config = mkIf cfg.enable {
    sound.enable = true;

    hardware.pulseaudio = {
      enable = true;
      support32Bit = true;
    };

    user.extraGroups = [ "audio" "pulse" ];
  };
}

