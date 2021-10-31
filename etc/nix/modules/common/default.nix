{ pkgs, ... }:

{
  imports = [ ./nixpkgs.nix ./options.nix ./home-manager.nix ];

  config = {
    environment.systemPackages = with pkgs; [
      file
      git
      neovim
      rclone
      unzip
      wget

      binutils
      coreutils
      pciutils
    ];
  };
}
