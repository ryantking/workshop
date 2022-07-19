{
  config,
  lib,
  pkgs,
  ...
}: {
  home.packages = [
    pkgs.yubikey-manager
    pkgs.yubikey-personalization
  ];

  programs.gpg.scdaemonSettings = {
    disable-ccid = true;
    reader-port = "Yubico Yubi";
  };

  services.gpg-agent.enableScDaemon = true;
}
