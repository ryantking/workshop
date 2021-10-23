{ config, lib, pkgs, options, ... }:

let
  inherit (lib) mkEnableOption mkIf;

  cfg = config.modules.hardware.audio;
in {
  options.modules.hardware.audio = { enable = mkEnableOption "audio support"; };

  config = mkIf cfg.enable {
    sound.enable = true;

    hardware.pulseaudio = {
      enable = true;
      daemon.config = {
        # avoid-resampling = true;
        default-sample-rate = 48000;
      };

      configFile = pkgs.runCommand "default.pa" {} ''
      sed 's/module-udev-detect$/module-udev-detect tsched=0/' \
      ${pkgs.pulseaudio}/etc/pulse/default.pa > $out
      '';
    };

    user.extraGroups = [ "audio" "pulse" ];
  };
}
