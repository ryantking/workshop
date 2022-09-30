{
  config,
  options,
  lib,
  pkgs,
  ...
}: let
  gnupgHome = "${config.xdg.dataHome}/gnupg";
  key = config.whoami.keys.pgp.primary;
in {
  home = {
    packages = with pkgs; [
      gnupg
      gpgme

      (writeShellScriptBin "gpg-agent-restart" ''
        pkill gpg-agent ; pkill ssh-agent ; pkill pinentry ; eval $(gpg-agent --daemon --enable-ssh-support)
      '')
    ];

    activation.ensureGnupgHome = ''
      sudo chown -R ${config.home.username} ${gnupgHome}
      find ${gnupgHome} -type f -exec sudo chmod 600 {} \;
      find ${gnupgHome} -type d -exec sudo chmod 700 {} \;
    '';
  };

  shell.env = {
    PGPKEYID = key;
    GNUPGHOME = gnupgHome;
    GPG_TTY = "$(tty)";
    SSH_AUTH_SOCK = "$(${pkgs.gnupg}/bin/gpgconf --list-dirs agent-ssh-socket)";
  };

  # services.gpg-agent = {
  #   enable = true;
  #   enableSshSupport = true;
  #   sshKeys = [config.whoami.keys.pgp.primary];
  # };

  programs.gpg = {
    enable = true;
    homedir = gnupgHome;
    mutableKeys = true;
    mutableTrust = true;

    settings.keyserver = "hkps://keys.openpgp.org";
  };

  xdg.dataFile."gnupg/gpg-agent.conf".text = ''
    pinentry-program ${
      if pkgs.stdenv.isDarwin
      then "${pkgs.pinentry_mac}/Applications/pinentry-mac.app/Contents/MacOS/pinentry-mac"
      else "${pkgs.pinentry-qt}/bin/pinentry-qt"
    }
    enable-ssh-support
    allow-emacs-pinentry
    allow-loopback-pinentry
  '';
}
