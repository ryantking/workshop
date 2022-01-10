{ lib, suites, ... }:

{
  networking.hostName = "trashbook";

  nixpkgs.system = "x86_64-darwin";

  nix = {
    maxJobs = 4;
    buildCores = 0;
  };

  my = {
    username = "rking";
    email = "ryantking@protonmail.com";
  };

  homebrew = {
    casks = [
      "1password-beta"
      "adobe-acrobat-reader"
      "adobe-creative-cloud"
      "alfred"
      "displaperture"
      "dropbox"
      "karabiner-elements"
      "logitech-options"
      "logseq"
      "pictogram"
      "protonvpn"
      "signal"
      "spotify"
      "zsa-wally"
    ];
    masApps = {
      "Fantastical" = 975937182;
      "Tailscale" = 1475387142;
      "Telegram" = 747648890;
      "Slack" = 803453959;
    };
  };

  my.hm.programs.ssh = {
    enable = true;
    extraConfig = "PasswordAuthentication no";
    hashKnownHosts = true;
    controlMaster = "auto";
    matchBlocks = {
      "*.redhat.com".extraOptions = {
        "GSSAPIAuthentication" = "yes";
        "GSSAPIDelegateCredentials" = "no";
      };
      "github.com" = {
        user = "git";
        identitiesOnly = true;
        identityFile = "~/.ssh/id_rsa_yubikey.pub";
        extraOptions = {
          "MACs" =
            "hmac-sha2-512-etm@openssh.com,hmac-sha2-256-etm@openssh.com,umac-128-etm@openssh.com,hmac-sha2-512,hmac-sha2-256,umac-128@openssh.com";
        };
      };
      "trashmetal" = {
        user = "root";
        identitiesOnly = true;
        identityFile = "~/.ssh/id_rsa_yubikey.pub";
        forwardAgent = true;
        extraOptions = { "ControlPersist" = "30m"; };
      };
      "trashstation" = {
        user = "rking";
        identitiesOnly = true;
        identityFile = "~/.ssh/id_rsa_yubikey.pub";
        extraOptions = {
          "StreamLocalBindUnlink" = "yes";
          "RemoteForward /run/user/1000/gnupg/S.gpg-agent" =
            "/Users/rking/.gnupg/S.gpg-agent.extra";
          "RemoteForward /run/user/1000/gnupg/S.gpg-agent.ssh" =
            "/Users/rking/.gnupg/S.gpg-agent.ssh";
          "ControlPersist" = "30m";
        };
      };
    };
  };
}
