{ config, lib, pkgs, ... }:

let
  inherit (builtins) toJSON;
  inherit (lib) elem mapAttrs filterAttrs mapAttrsToList;
  inherit (pkgs.lib.our) mkAnsiAlias;

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

  mkAccount = name: { username, flavor, domain ? "gmail.com", extraChannels ? { }, ... }@account: rec {
    address = "${username}@${domain}";
    realName = config.whoami.name;
    userName = address;
    passwordCommand = "${config.programs.password-store.package}/bin/pass 'email/${address}'";

    maildir.path = name;
    neomutt.enable = true;
    himalaya.enable = true;
    msmtp.enable = true;
    mu.enable = true;

    goimapnotify = {
      enable = true;
      onNewMailPost = "if mu index --lazy-check; " +
        "then test -f /tmp/mu_reindex_now && rm /tmp/mu_reindex_now; " +
        "else touch /tmp/mu_reindex_now; fi";
    };

    mbsync = {
      enable = true;
      create = "maildir";
      remove = "none";
      expunge = "both";
      groups.${name}.channels = mapAttrs
        (_: v: v // {
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
        } // extraChannels);
    };
  } // (filterAttrs (n: _: ! elem n [ "username" "domain" "extraChannels" ]) account);
in
{
  accounts.email = {
    maildirBasePath = "${config.home.homeDirectory}/Mail";
    accounts = (mapAttrs mkAccount {
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
        extraChannels = {
          sent = {
            farPattern = "Sent";
            nearPattern = "sent";
            extraConfig.Expunge = "Both";
          };
          trash = {
            farPattern = "Trash";
            nearPattern = "trash";
            extraConfig.Expunge = "Both";
          };
          spam = {
            farPattern = "Spam";
            nearPattern = "spam";
            extraConfig.Expunge = "Both";
          };
        };
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
    });
  };

  programs = {
    mu.enable = true;
    mbsync.enable = true;
    msmtp.enable = true;
    himalaya.enable = true;

    neomutt = {
      enable = true;
      vimKeys = true;
      sidebar.enable = true;
      extraConfig = ''
        color normal    default default             # default colours
        color index    brightblue default ~N       # new messages
        color index    red default ~F              # flagged messages
        color index    blue default ~T             # tagged messages
        color index    brightred default ~D        # deleted messages
        color body    brightgreen default         (https?|ftp)://[\-\.+,/%~_:?&=\#a-zA-Z0-9]+  # links
        color body    brightgreen default         [\-\.+_a-zA-Z0-9]+@[\-\.a-zA-Z0-9]+          # email-addresses
        color attachment  magenta default             # attachments
        color signature    brightwhite default         # sigs
        color search    brightred black             # highlight results

        color indicator    black cyan                  # currently highlighted message
        color error    red default                 # error messages
        color status    white brightblack           # status line
        color tree    white default               # thread tree arrows
        color tilde    cyan default                # blank line padding

        color hdrdefault  brightblue default          # default headers
        color header    cyan default "^From:"
        color header     cyan default "^Subject:"

        color quoted    cyan default                # quote colours
        color quoted1    brightcyan default
        color quoted2    blue default
        color quoted3    green default
        color quoted4    yellow default
        color quoted5    red default
      '';
    };
  };

  services = {
    mbsync = {
      enable = true;
      logDirectory = "${config.xdg.cacheHome}/logs/org.nixos/home";
    };

    goimapnotify.enable = true;
  };

  shell.aliases = {
    mutt = mkAnsiAlias "neomutt";
    neomutt = mkAnsiAlias "neomutt";
  };
}
