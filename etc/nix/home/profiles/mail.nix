{
  config,
  lib,
  pkgs,
  ...
}: let
  maildir = "${config.home.homeDirectory}/Mail";

  muIndex = pkgs.writeShellScript "mu-index" ''
    if ${pkgs.mu}/bin/mu index --lazy-check; then
      test -f /tmp/mu_reindex_now && rm /tmp/mu_reindex_now
    else
      touch /tmp/mu_reindex_now
    fi
  '';

  protonmailExtraChannels = {
    trash = {
      farPattern = "Trash";
      nearPattern = "trash";
    };
    sent = {
      farPattern = "Sent";
      nearPattern = "sent";
      extraConfig.Expunge = "Both";
    };
    archive = {
      farPattern = "Archive";
      nearPattern = "archive";
    };
    drafts = {
      farPattern = "Drafts";
      nearPattern = "drafts";
      extraConfig.Expunge = "Both";
    };
  };

  gmailExtraChannels = {
    trash = {
      farPattern = "[Gmail]/Trash";
      nearPattern = "trash";
    };
    sent = {
      farPattern = "[Gmail]/Sent Mail";
      nearPattern = "sent";
      extraConfig.Expunge = "Both";
    };
    archive = {
      farPattern = "[Gmail]/All Mail";
      nearPattern = "archive";
    };
    starred = {
      farPattern = "[Gmail]/Starred";
      nearPattern = "starred";
    };
    drafts = {
      farPattern = "[Gmail]/Drafts";
      nearPattern = "drafts";
      extraConfig.Expunge = "Both";
    };
  };

  mkGoImapNotifyConfig = name: {
    enable = true;
    onNewMail = "${pkgs.isync}/bin/mbsync ${name}-%s";
    onNewMailPost = "${muIndex}";
  };

  mkAccount = name: {
    username,
    flavor,
    domain ? "gmail.com",
    extraChannels ? {},
    ...
  } @ account:
    rec {
      address = "${username}@${domain}";
      realName = config.whoami.name;
      userName = address;
      passwordCommand = "${pkgs.pass}/bin/pass 'email/${address}'";

      mu.enable = true;
      msmtp.enable = true;
      goimapnotify = mkGoImapNotifyConfig name;

      mbsync = {
        enable = true;
        create = "maildir";
        remove = "none";
        expunge = "both";
        groups.${name}.channels =
          lib.mapAttrs
          (_: v:
            v
            // {
              extraConfig = {
                Create = "Near";
                CopyArrivalDate = "yes";
                Sync = "All";
                SyncState = "*";
              };
            })
          ({
              inbox = {
                farPattern = "";
                nearPattern = "inbox";
                extraConfig.Expunge = "Both";
              };
            }
            // extraChannels);
      };
    }
    // (lib.filterAttrs (n: _: ! lib.elem n ["username" "domain" "extraChannels"]) account);
in {
  accounts.email = {
    maildirBasePath = "${config.home.homeDirectory}/Mail";
    accounts = lib.mapAttrs mkAccount {
      personal = {
        username = "ryantking";
        domain = "protonmail.com";
        flavor = "plain";
        primary = true;
        imap = {
          host = "127.0.0.1";
          port = 1143;
          tls.enable = false;
        };
        smtp = {
          host = "127.0.0.1";
          port = 1025;
          tls.enable = true;
        };
        extraChannels = protonmailExtraChannels;
      };
      gmail = {
        username = "ryan.taylor.king";
        flavor = "gmail.com";
        extraChannels = gmailExtraChannels;
      };
      work = {
        username = "ryking";
        domain = "redhat.com";
        flavor = "gmail.com";
        extraChannels = gmailExtraChannels;
      };
    };
  };

  programs = {
    mu.enable = true;
    mbsync.enable = true;
    msmtp.enable = true;
  };

  services = {
    mbsync.enable = true;
    goimapnotify.enable = true;
  };

  launchd.agents.mbsync.config = {
    EnvironmentVariables = {
      PASSWORD_STORE_DIR = "${config.xdg.dataHome}/pass";
      GNUPGHOME = "${config.xdg.dataHome}/gnupg";
      PASSWORD_STORE_GPG_OPTS = "-u 0x7B9DDE8739045EF3";
    };
    StandardOutPath = "${config.xdg.cacheHome}/logs/org.nixos/home/mbsync.out.log";
    StandardErrorPath = "${config.xdg.cacheHome}/logs/org.nixos/home/mbsync.err.log";
  };

  launchd.agents.goimapnotify.config = {
    EnvironmentVariables = {
      PASSWORD_STORE_DIR = "${config.xdg.dataHome}/pass";
      GNUPGHOME = "${config.xdg.dataHome}/gnupg";
      PASSWORD_STORE_GPG_OPTS = "-u 0x7B9DDE8739045EF3";
    };
    StandardOutPath = "${config.xdg.cacheHome}/logs/org.nixos/home/goimapnotify.out.log";
    StandardErrorPath = "${config.xdg.cacheHome}/logs/org.nixos/home/goimapnotify.err.log";
  };
}
