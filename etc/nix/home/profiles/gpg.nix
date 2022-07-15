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
  programs.gpg = {
    enable = true;
    homedir = gnupgHome;

    scdaemonSettings = {
      disable-ccid = true;
      reader-port = ''"Yubico YubiKey OTP+FIDO+CCID"'';
    };
  };

  shell = {
    env = {
      PGPKEYID = key;
      GNUPGHOME = gnupgHome;
      GPG_TTY = "$(tty)";
      SSH_AUTH_SOCK = "$(gpgconf --list-dirs agent-ssh-socket)";
    };

    extraInit = "gpgconf --launch gpg-agent";
  };

  home = {
    packages = with pkgs; [
      yubikey-manager
      yubikey-personalization
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

  xdg.dataFile."gnupg/gpg.conf".text = ''
    # https://github.com/drduh/config/blob/master/gpg.conf
    # https://www.gnupg.org/documentation/manuals/gnupg/GPG-Configuration-Options.html
    # https://www.gnupg.org/documentation/manuals/gnupg/GPG-Esoteric-Options.html
    # Use AES256, 192, or 128 as cipher
    personal-cipher-preferences AES256 AES192 AES
    # Use SHA512, 384, or 256 as digest
    personal-digest-preferences SHA512 SHA384 SHA256
    # Use ZLIB, BZIP2, ZIP, or no compression
    personal-compress-preferences ZLIB BZIP2 ZIP Uncompressed
    # Default preferences for new keys
    default-preference-list SHA512 SHA384 SHA256 AES256 AES192 AES ZLIB BZIP2 ZIP Uncompressed
    # SHA512 as digest to sign keys
    cert-digest-algo SHA512
    # SHA512 as digest for symmetric ops
    s2k-digest-algo SHA512
    # AES256 as cipher for symmetric ops
    s2k-cipher-algo AES256
    # UTF-8 support for compatibility
    charset utf-8
    # Show Unix timestamps
    fixed-list-mode
    # No comments in signature
    no-comments
    # No version in output
    no-emit-version
    # Disable banner
    no-greeting
    # Long hexidecimal key format
    keyid-format 0xlong
    # Display UID validity
    list-options show-uid-validity
    verify-options show-uid-validity
    # Display all keys and their fingerprints
    with-fingerprint
    # Cross-certify subkeys are present and valid
    require-cross-certification
    # Disable caching of passphrase for symmetrical ops
    no-symkey-cache
    # Enable smartcard
    use-agent
    # Group recipient keys (preferred ID last)
    # group keygroup = 0xFF00000000000001 0xFF00000000000002 ${key}
    # Keyserver URL
    keyserver hkps://keys.openpgp.org
    # Show verbose output
    verbose
  '';

  xdg.dataFile."gnupg/gpg-agent.conf".text = ''
    # https://github.com/drduh/config/blob/master/gpg-agent.conf
    # https://www.gnupg.org/documentation/manuals/gnupg/Agent-Options.html
    enable-ssh-support
    allow-loopback-pinentry
    ttyname $GPG_TTY
    default-cache-ttl 60
    max-cache-ttl 120
    pinentry-program ${
      if pkgs.stdenv.isDarwin
      then "/usr/local/bin/pinentry-mac"
      else "/usr/bin/pinentry-curses"
    }
  '';
}
