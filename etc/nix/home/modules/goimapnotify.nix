{
  config,
  options,
  lib,
  pkgs,
  ...
}: let
  inherit (lib) types;
  inherit (pkgs.lib.our) mkOpt;

  accounts = lib.filterAttrs (_: a: a.goimapnotify.enable) config.accounts.email.accounts;

  mkConfigEntry = _: {
    userName,
    flavor,
    imap,
    goimapnotify,
    ...
  }:
    {
      inherit (goimapnotify) onNewMail onNewMailPost boxes;
      username = userName;
      passwordCmd = "${config.programs.password-store.package}/bin/pass 'email/${userName}'";
    }
    // (
      if flavor == "gmail.com"
      then {
        host = "imap.gmail.com";
        port = 993;
        tls = true;
        tlsOption.reject_unauthorized = true;
      }
      else {
        inherit (imap) host port;
      }
    );

  cfg = config.services.goimapnotify;
in {
  options = {
    accounts.email.accounts = lib.mkOption {
      type = types.attrsOf (types.submodule {
        options.goimapnotify = {
          enable = lib.mkEnableOption "goimapnotify";
          onNewMail = mkOpt types.str "${pkgs.isync}/bin/mbsync %s";
          onNewMailPost = mkOpt types.str "${pkgs.notmuch}/bin/notmuch new";
          boxes = mkOpt (types.listOf types.attrs) [{mailbox = "inbox";}];
        };
      });
    };

    services.goimapnotify = {
      enable = lib.mkEnableOption "goimapnotify";
      package = mkOpt types.package pkgs.goimapnotify;
    };
  };

  config = lib.mkMerge [
    {
      home.packages = lib.optional cfg.enable cfg.package;

      xdg.configFile."goimapnotify.conf" = lib.optionalAttrs cfg.enable {
        text = builtins.toJSON (lib.mapAttrsToList mkConfigEntry accounts);
      };
    }
    (lib.optionalAttrs (lib.hasAttr "launchd" options) {
      launchd.agents = {
        goimapnotify = lib.optionalAttrs cfg.enable {
          enable = true;
          config = {
            ProgramArguments = ["${cfg.package}/bin/goimapnotify" "-conf" "${config.xdg.configHome}/goimapnotify.conf"];
            ProcessType = "Adaptive";
            RunAtLoad = true;
            KeepAlive = true;
          };
        };
      };
    })
  ];
}
