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
          "RemoteForward /run/user/1000/emacs/server" =
            "/Users/rking/.config/emacs/servers/server";
          "ControlPersist" = "30m";
        };
      };
    };
  };

  theme = {
    colorscheme = "nord";

    fonts = with pkgs; rec {
      mono = {
        family = "Victor Mono";
        style = "Regular";
        size = 12;
        pkg = victor-mono;
      };
      nerdfont = {
        family = "VictorMono Nerd Font";
        style = "Regular";
        size = 12;
        pkg = (nerdfonts.override { fonts = [ "VictorMono" ]; });
      };
    };
  };
}
