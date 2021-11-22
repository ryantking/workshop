{ inputs, pkgs, ... }:

{
  user = {
    description = "Ryan King";
    name = "rking";
    home = "/Users/rking";
  };

  hm.programs.ssh = {
    enable = true;
    extraConfig = "PasswordAuthentication no";
    hashKnownHosts = true;
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
        };
      };
    };
  };

  theme = {
    colorscheme = "nord";

    fonts = rec {
      mono = {
        family = "Monoid Nerd Font";
        style = "Retina";
        size = 12;
        pkg = pkgs.nerdfonts.override;
      };
    };
  };
}
