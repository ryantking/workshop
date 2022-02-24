{ config, options, lib, pkgs, ... }:

let
  gpgSuiteHome = "/usr/local/MacGPG2";
in
{
  homebrew = {
    brews = [ "pinentry-mac" ];
    casks = [ "gpg-suite" "yubico-yubikey-personalization-gui" ];
  };

  environment.systemPath = [ "${gpgSuiteHome}/bin" ];

  launchd.user.agents.gnupg-agent.serviceConfig = let inherit (config.home) cacheHome; in
    {
      ProgramArguments = [
        "${gpgSuiteHome}/bin/gpg-connect-agent"
        "/bye"
      ];
      RunAtLoad = true;
      KeepAlive = true;
      StandardOutPath = "${cacheHome}/logs/gpg-agent.out.log";
      StandardErrorPath = "${cacheHome}/logs/gpg-agent.err.log";
    };

  home.dataFile = {
    "gnupg/gpg-agent.conf".text = ''
      pinentry-program /usr/local/bin/pinentry-mac
    '';
  };
}
