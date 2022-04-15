{ hmUsers, inputs, config, options, lib, pkgs, ... }:

let
  inherit (builtins) elem getEnv baseNamefOf;
  inherit (inputs) colors;
  inherit (lib) mkMerge optionalAttrs mkAliasDefinitions;
  inherit (pkgs.stdenv) isDarwin isLinux;
  inherit (config) username;
in
{
  users.users.${username} = {
    name = username;
    home = if isDarwin then "/Users/${username}" else "/home/${username}";
    shell = pkgs.zsh;
    packages = import ./packages.nix { inherit pkgs; };
  } // (optionalAttrs (isLinux) {
    uid = 1000;
    isNormalUser = true;
    extraGroups = [ "wheel" "networkmanager" "audio" "pulse" ];
    hashedPassword =
      "$6$xL0fzTOIJV5KEQ$clkH7gC8TThDI/2cqBmpi2eVDH5JTWXUMlPnh4Qwq3LhmB9tSwrlPlgF51V0lXBZtzyQnuQJX4.hM0pr2JcpV0";
    openssh.authorizedKeys.keys = [ config.user.keys.ssh ];
  });

  environment.variables = let inherit (config.user) xdg; in
    {
      CACHEDIR = xdg.cache;
      XDG_BIN_HOME = xdg.bin;
      XDG_CACHE_HOME = xdg.cache;
      XDG_CONFIG_HOME = xdg.config;
      XDG_DATA_HOME = xdg.data;
      XDG_RUNTIME_DIR = "/tmp";
      XDG_STATE_HOME = xdg.state;
    };

  home-manager = {
    useGlobalPkgs = true;
    useUserPackages = true;
    users.${username} = hmUsers.${username};
  };

  security = (optionalAttrs isLinux { sudo.wheelNeedsPassword = false; });
}
