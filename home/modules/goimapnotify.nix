{ config, options, lib, pkgs, ... }:

let
  inherit (builtins) elem toJSON;
  inherit (lib) filterAttrs hasAttr optional mapAttrsToList attrValues mkEnableOption mkOption mkMerge optionalAttrs types;
  inherit (pkgs.stdenv) isDarwin;
  inherit (pkgs.lib.our) mkOpt;

  accounts = filterAttrs (_: a: a.goimapnotify.enable) config.accounts.email.accounts;

  mkConfigEntry = _: { userName, flavor, imap, goimapnotify, ... }: {
    inherit (goimapnotify) onNewMail onNewMailPost boxes;
    username = userName;
    tls = true;
    tlsOption.reject_unauthorized = true;
    passwordCmd = "${config.programs.password-store.package}/bin/pass 'email/${userName}'";
  } // (if flavor == "gmail.com" then {
    host = "imap.gmail.com";
    port = 993;
  } else {
    inherit (imap) host port;
  });

  cfg = config.services.goimapnotify;
in
{
  options = {
    accounts.email.accounts = mkOption {
      type = types.attrsOf (types.submodule {
        options.goimapnotify = {
          enable = mkEnableOption "goimapnotify";
          onNewMail = mkOpt types.str "${config.services.mbsync.package}/bin/mbsync test-%s";
          onNewMailPost = mkOpt types.str "mu index --lazy-check";
          boxes = mkOpt (types.listOf types.attrs) [{ mailbox = "INBOX"; }];
        };
      });
    };

    services.goimapnotify = {
      enable = mkEnableOption "goimapnotify";
      package = mkOpt types.package pkgs.goimapnotify;
      logDirectory = mkOpt types.path "${config.xdg.cacheHome}/logs/org.nixos/home";
    };
  };

  config = mkMerge [
    {
      home.packages = (optional cfg.enable cfg.package);

      xdg.configFile."goimapnotify.conf" = optionalAttrs cfg.enable {
        text = toJSON (mapAttrsToList mkConfigEntry accounts);
      };
    }
    (optionalAttrs (hasAttr "launchd" options) {
      launchd.agents = {
        #   mbsync = (optionalAttrs mbsynccfg.enable {
        #     enable = true;
        #     config = {
        #       ProgramArguments = [ "${mbsynccfg.package}/bin/mbsync" "-a" ];
        #       EnvironmentVariables.GNUPGHOME = "${config.xdg.dataHome}/gnupg";
        #       ProcessType = "Adaptive";
        #       RunAtLoad = true;
        #       StartInterval = 60 * 15;
        #       StandardOutPath = "${mbsynccfg.logDir}/mbsync.out.log";
        #       StandardErrorPath = "${mbsynccfg.logDir}/mbsync.err.log";
        #     };
        #   });

        goimapnotify = (optionalAttrs cfg.enable {
          enable = true;
          config = {
            ProgramArguments = [ "${cfg.package}/bin/goimapnotify" "-conf" "${config.xdg.configHome}/goimapnotify.conf" ];
            EnvironmentVariables.GNUPGHOME = "${config.xdg.dataHome}/gnupg";
            RunAtLoad = true;
            KeepAlive = true;
            ProcessType = "Adaptive";
            StandardOutPath = "${cfg.logDirectory}/goimapnotify.out.log";
            StandardErrorPath = "${cfg.logDirectory}/goimapnotify.err.log";
          };
        });
      };
    })
  ];
}
