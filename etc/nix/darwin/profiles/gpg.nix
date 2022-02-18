{ config, options, lib, pkgs, ... }:

let
  gpgSuiteHome = "/usr/local/MacGPG2";
in
{
  homebrew = {
    brews = [ "pinentry-mac" ];
    casks = [ "gpg-suite" "yubico-yubikey-personalization-gui" ];
  };

  environment = {
    systemPath = [ "${gpgSuiteHome}/bin" ];

    extraInit = ''
      # Bind gpg-agent to this TTY if gpg commands are used.
      export GPG_TTY=$(tty)
      # SSH agent protocol doesn't support changing TTYs, so bind the agent
      # to every new TTY.
      ${gpgSuiteHome}/bin/gpg-connect-agent --quiet updatestartuptty /bye > /dev/null
      export SSH_AUTH_SOCK=$(${gpgSuiteHome}/bin/gpgconf --list-dirs agent-ssh-socket)
    '';
  };

  launchd.user.agents.gnupg-agent.serviceConfig = {
    ProgramArguments = [
      "${gpgSuiteHome}/bin/gpg-connect-agent"
      "/bye"
    ];
    RunAtLoad = true;
    KeepAlive = true;
  };


  home.dataFile = {
    "gnupg/gpg-agent.conf".text = ''
      pinentry-program /usr/local/bin/pinentry-mac
    '';
  };
}
