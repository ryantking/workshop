{
  config,
  pkgs,
  ...
}: let
  identityFileName = "id_rsa_yk.pub";
  identityFile = "~/.ssh/${identityFileName}";
in {
  home.file.".ssh/${identityFileName}".text = config.whoami.keys.ssh.primary;

  programs.ssh = {
    enable = true;
    hashKnownHosts = true;
    controlMaster = "auto";

    matchBlocks = {
      "github.com" = {
        inherit identityFile;
        user = "git";
        identitiesOnly = true;
      };

      "charlemagne" = {
        inherit identityFile;
        identitiesOnly = true;
        forwardAgent = true;
        extraOptions = {"ControlPersist" = "30m";};
      };

      "rome" = {
        inherit identityFile;
        user = "root";
        identitiesOnly = true;
        forwardAgent = true;
        extraOptions = {"ControlPersist" = "30m";};
      };

      "thevatican" = {
        inherit identityFile;
        identitiesOnly = true;
      };

      "templar" = {
        inherit identityFile;
        user = "root";
        identitiesOnly = true;
      };

      "*" = {
        addressFamily = "inet";
        forwardX11 = false;
        forwardX11Trusted = false;
        serverAliveInterval = 300;
        serverAliveCountMax = 2;

        extraOptions = {
          AddKeysToAgent = "yes";
          ChallengeResponseAuthentication = "no";
          StrictHostKeyChecking = "ask";
          VerifyHostKeyDNS = "yes";
          VisualHostKey = "yes";

          Ciphers = "chacha20-poly1305@openssh.com,aes256-gcm@openssh.com";
          HostKeyAlgorithms = "ssh-ed25519-cert-v01@openssh.com,ssh-rsa-cert-v01@openssh.com,ssh-ed25519,ssh-rsa";
          KexAlgorithms = "curve25519-sha256@libssh.org,diffie-hellman-group-exchange-sha256";
          MACs = "hmac-sha2-512-etm@openssh.com,hmac-sha2-256-etm@openssh.com";
        };
      };
    };
  };
}
