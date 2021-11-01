{ inputs, config, pkgs, ... }:

{
  imports = [ ./brew.nix ./apps.nix ./yabai.nix ./alacritty.nix ];

  config = {
    system.stateVersion = 4;
    users.nix.configureBuildUsers = true;
    programs.zsh.enable = true;
    services.nix-daemon.enable = true;

    environment = {
      pathsToLink = [ "/Applications" ];
      etc = { darwin.source = "${inputs.darwin}"; };
    };

    nix = {
      gc.user = "${config.user.name}";
      useDaemon = true;
      nixPath = [ "darwin=/etc/${config.environment.etc.darwin.target}" ];
      extraOptions = ''
        extra-platforms = x86_64-darwin
      '';
    };
  };
}
