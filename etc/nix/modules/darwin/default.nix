{ inputs, config, pkgs, ... }:

let inherit (pkgs) writeShellScriptBin;
in {
  imports = [ ./brew.nix ./apps.nix ./emacs.nix ./yabai.nix ];

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
      useDaemon = true;
      nixPath = [ "darwin=/etc/${config.environment.etc.darwin.target}" ];
      extraOptions = ''
        extra-platforms = x86_64-darwin
      '';

      gc.user = "${config.user.name}";
    };
  };
}
