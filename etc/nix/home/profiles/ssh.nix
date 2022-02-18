{ config, ... }:

{
  programs.ssh = {
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
        user = config.home.username;
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
