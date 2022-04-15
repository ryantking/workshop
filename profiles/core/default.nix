{ config, lib, pkgs, ... }:

let
  inherit (pkgs.stdenv) isDarwin;
in
{
  imports = [ ./cachix.nix ./nix.nix ./shell.nix ];

  environment = {
    variables = {
      KERNEL_NAME = if pkgs.stdenv.isDarwin then "darwin" else "linux";
      LANG = "en_US.UTF-8";
      LC_ALL = "en_US.UTF-8";
      HOSTNAME = config.networking.hostName;
      TMPDIR = "/tmp";
      WORKSHOP_DIR = config.workshop.dir;
    };

    shells = with pkgs; [ bashInteractive dash zsh ];

    systemPackages = with pkgs; [
      (python3.withPackages (ps: with ps; [ pip setuptools ]))
      (ripgrep.override { withPCRE2 = true; })

      cachix
      (if isDarwin then coreutils-prefixed else coreutils)
      curl
      dash
      dnsutils
      findutils
      gawk
      gnumake
      grc
      less
      lua
      manix
      more
      moreutils
      neovim
      nix-index
      nix-tree
      nmap
      nvfetcher
      openssh
      openssl
      rsync
      skim
      tealdeer
      tmux
      wget
      whois
    ];
  };
}
