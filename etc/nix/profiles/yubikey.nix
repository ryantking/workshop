{
  config,
  lib,
  pkgs,
  ...
}: {
  services.pcscd.enable = true;
  services.udev.packages = [pkgs.yubikey-personalization];

  programs = {
    ssh.startAgent = false;
    gnupg.agent = {
      enable = true;
      enableSSHSupport = true;
    };
  };
}
