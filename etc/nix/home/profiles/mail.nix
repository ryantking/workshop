{
  config,
  lib,
  pkgs,
  ...
}: let
  maildir = "${config.home.homeDirectory}/Mail";

  passEnv = {
    PASSWORD_STORE_DIR = "${config.workshop.stateHome}/pass";
    PASSWORD_STORE_KEY = "${config.whoami.keys.pgp.machine}";
    GNUPGHOME = "${config.xdg.dataHome}/gnupg";
  };

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

      notmuch.enable = true;
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
    maildirBasePath = maildir;
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
        username = "ryan.king";
        domain = "flipsidecrypto.com";
        flavor = "gmail.com";
        extraChannels = gmailExtraChannels;
      };
    };
  };

  programs = {
    afew.enable = true;
    mbsync.enable = true;
    # msmtp.enable = true;

    notmuch = {
      enable = true;

      hooks = {
        postNew = "${pkgs.afew}/bin/afew --tag --new";
      };
    };
  };

  services = {
    mbsync.enable = true;
    goimapnotify.enable = true;
  };

  launchd.agents = {
    mbsync.config = {
      EnvironmentVariables = passEnv;
      StandardOutPath = "${config.xdg.cacheHome}/logs/mbsync.log";
      StandardErrorPath = "${config.xdg.cacheHome}/logs/mbsync.log";
    };

    notmuch = {
      enable = true;
      config = {
        ProgramArguments = ["${pkgs.notmuch}/bin/notmuch" "new"];
        EnvironmentVariables.NOTMUCH_CONFIG = "${config.xdg.configHome}/notmuch/default/config";
        WorkingDirectory = maildir;
        StartInterval = 60 * 2;
        ProcessType = "Adaptive";
        RunAtLoad = true;
        KeepAlive = true;
        StandardOutPath = "${config.xdg.cacheHome}/logs/notmuch.log";
        StandardErrorPath = "${config.xdg.cacheHome}/logs/notmuch.log";
      };
    };

    goimapnotify.config = {
      EnvironmentVariables =
        {
          NOTMUCH_CONFIG = "${config.xdg.configHome}/notmuch/default/config";
        }
        // passEnv;
      StandardOutPath = "${config.xdg.cacheHome}/logs/goimapnotify.log";
      StandardErrorPath = "${config.xdg.cacheHome}/logs/goimapnotify.log";
    };
  };
}
