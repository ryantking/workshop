{ inputs, config, pkgs, ... }:

{
  imports = [ ./brew.nix ./apps.nix ./yabai.nix ];

  config = {
    system.stateVersion = 4;
    users.nix.configureBuildUsers = true;
    programs.zsh.enable = true;
    services.nix-daemon.enable = true;

    launchd.user.agents."emacsd" = let cfg = config.emacs;
    in {
      command = "/usr/local/bin/emacs --fg-daemon";
      environment = {
        EMACSDIR = cfg.configDir;
        DOOMDIR = cfg.doom.configDir;
        DOOMLOCALDIR = cfg.doom.dataDir;
      };
      serviceConfig.RunAtLoad = true;
    };

    environment = {
      pathsToLink = [ "/Applications" ];
      etc = { darwin.source = "${inputs.darwin}"; };
    };

    nix = {
      useDaemon = true;
      nixPath = [ "darwin=/etc/${config.environment.etc.darwin.target}" ];
      extraOptions = ''
        extra-platforms = x86_64-darwin
      '';

      gc.user = "${config.user.name}";
    };
  };
}
