{
  hmUsers,
  config,
  lib,
  pkgs,
  ...
}: let
  inherit (config) username;
in {
  users.users.${username} =
    {
      name = username;
      home =
        if pkgs.stdenv.isDarwin
        then "/Users/${username}"
        else "/home/${username}";
      shell = pkgs.zsh;
      packages = import ./packages.nix {inherit pkgs;};
    }
    // (lib.optionalAttrs pkgs.stdenv.isLinux {
      uid = 1000;
      isNormalUser = true;
      extraGroups = ["wheel" "networkmanager" "audio" "pulse"];
      hashedPassword = "$6$xL0fzTOIJV5KEQ$clkH7gC8TThDI/2cqBmpi2eVDH5JTWXUMlPnh4Qwq3LhmB9tSwrlPlgF51V0lXBZtzyQnuQJX4.hM0pr2JcpV0";
      openssh.authorizedKeys.keys = config.whoami.keys.ssh.identities;
    });

  home-manager = {
    useGlobalPkgs = true;
    useUserPackages = true;
    users.${username} = hmUsers.${username};
  };

  security = lib.optionalAttrs pkgs.stdenv.isLinux {sudo.wheelNeedsPassword = false;};
}
