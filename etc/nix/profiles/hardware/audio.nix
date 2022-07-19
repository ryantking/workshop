{
  config,
  lib,
  pkgs,
  ...
}: {
  sound.enable = true;

  hardware.pulseaudio.enable = false;

  services.pipewire = {
    enable = true;

    pulse.enable = true;

    alsa = {
      enable = true;
      support32Bit = true;
    };
  };

  # hardware.pulseaudio = {
  #   enable = true;
  #   package = pkgs.pulseaudioFull;
  #   daemon.config.default-sample-rate = 48000;
  #   configFile = pkgs.runCommand "default.pa" {} ''
  #     sed 's/module-udev-detect$/module-udev-detect tsched=0/' \
  #     ${pkgs.pulseaudio}/etc/pulse/default.pa > $out
  #   '';
  # };
}
