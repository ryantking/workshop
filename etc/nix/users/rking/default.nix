{
  config,
  lib,
  pkgs,
  hmUsers,
  ...
}: let
  inherit (config.whoami.keys) ssh;
in {
  home-manager.users = {inherit (hmUsers) rking;};

  users.users.rking =
    {
      home =
        if pkgs.stdenv.isDarwin
        then "/Users/rking"
        else "/home/rking";
      shell = pkgs.zsh;
      packages = import ./packages.nix {inherit pkgs;};
    }
    // (lib.optionalAttrs pkgs.stdenv.isLinux {
      uid = 1000;
      isNormalUser = true;
      hashedPassword = "$6$xL0fzTOIJV5KEQ$clkH7gC8TThDI/2cqBmpi2eVDH5JTWXUMlPnh4Qwq3LhmB9tSwrlPlgF51V0lXBZtzyQnuQJX4.hM0pr2JcpV0";
      extraGroups = ["wheel" "networkmanager" "audio" "pulse"];
      openssh.authorizedKeys.keys = [ssh.primary] ++ ssh.identities;
    });
}
