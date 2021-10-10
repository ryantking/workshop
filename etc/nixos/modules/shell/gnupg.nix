{ config, lib, pkgs, ... }:

let
  inherit (lib) mkEnableOption mkIf mkMerge mkOption types;

  cfg = config.modules.shell.gnupg;
in {
  options.modules.shell.gnupg = {
    enable = mkEnableOption "GnuPG shell";
    sshAgent = mkEnableOption "GnuPG as SSH agent";
    cacheTTL = mkOption {
      type = types.int;
      default = 300;
    };
  };

  config = mkIf cfg.enable (mkMerge [
    {
      programs.gnupg.agent.enable = true;

      environment.shellInit = ''
        export GPG_TTY="$(tty)"
        gpg-connect-agent /bye
      '';

      hm = {
        programs.gpg.enable = true;

        services.gpg-agent = {
          enable = true;
          defaultCacheTtl = cfg.cacheTTL;
          pinentryFlavor = "curses";
        };

        home.packages = with pkgs; [
          pinentry-curses
        ];
      };
    }

    (mkIf cfg.sshAgent {
      programs = {
        gnupg.agent.enableSSHSupport = true;
        ssh.startAgent = false;
      };

      hm.services.gpg-agent = {
        enableSshSupport = true;
        sshKeys = [ "74CB8154D23D820537432F4B73446CF35A14216F" ];
      };

      environment.shellInit = ''
        export SSH_AUTH_SOCK="/run/user/$UID/gnupg/S.gpg-agent.ssh"
      '';
    })
  ]);
}
