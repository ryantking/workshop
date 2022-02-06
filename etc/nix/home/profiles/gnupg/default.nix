{ config, options, lib, pkgs, ... }:

let
  inherit (config) my;
  inherit (lib) hasAttr optional optionalAttrs mkMerge;
  inherit (pkgs.stdenv) isDarwin isLinux;

  gnupgHome = "${my.xdg.data}/gnupg";
  key = my.keys.pgp;
in
mkMerge [
  {
    programs.gnupg.agent = {
      enable = true;
      enableSSHSupport = true;
    };

    environment.systemPackages = with pkgs; [
      pinentry_mac
      (writeShellScriptBin "gpg-agent-restart" ''
        pkill gpg-agent ; pkill ssh-agent ; pkill pinentry ; eval $(gpg-agent --daemon --enable-ssh-support)
      '')
    ];

    system.activationScripts.postUserActivation.text = ''
      sudo chown -R ${my.username} ${gnupgHome}
      find ${gnupgHome} -type f -exec sudo chmod 600 {} \;
      find ${gnupgHome} -type d -exec sudo chmod 700 {} \;
    '';

    my = {
      env = {
        PGPKEYID = key;
        GNUPGHOME = gnupgHome;
      };

      hm = {
        xdg.dataFile = {
          "gnupg/gpg-agent.conf".text = ''
            pinentry-program ${my.user.home}/${pkgs.pinentry_mac.binaryPath}
          '';

          "gnupg/gpg.conf".text = ''
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
            # Disable recipient key ID in messages
            throw-keyids
            # Default/trusted key ID to use (helpful with throw-keyids)
            default-key ${key}
            trusted-key ${key}
            # Group recipient keys (preferred ID last)
            group keygroup = 0xFF00000000000001 0xFF00000000000002 ${key}
            # Keyserver URL
            keyserver hkps://keys.openpgp.org
          '';
        };
      };
    };
  }
  (optionalAttrs (hasAttr "homebrew" options) {
    homebrew.casks = [ "gpg-suite" ];
    my.env.PATH = [ "/usr/local/MacGPG2/bin" "$PATH" ];
  })
]
