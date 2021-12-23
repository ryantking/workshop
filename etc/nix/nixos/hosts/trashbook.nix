{ inputs, pkgs, ... }:

{

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

  theme = {
    colorscheme = "nord";

    fonts = with pkgs; rec {
      sans = {
        family = "Overpass";
        style = "Regular";
        size = 12;
        pkg = overpass;
      };
      serif = {
        family = "IBM Plex Mono";
        style = "Light";
        size = 12;
        pkg = ibm-plex;
      };
      ui = sans;
      mono = {
        family = "Victor Mono";
        style = "Regular";
        size = 12;
        pkg = victor-mono;
        nerdfont = {
          family = "VictorMono Nerd Font";
          pkg = (nerdfonts.override { fonts = [ "VictorMono" ]; });
        };
      };
      unicode = {
        family = "JuliaMono";
        style = "Regular";
        size = 12;
        pkg = julia-mono;
      };
    };
  };
}
