{ config, lib, pkgs, ... }:

{
  imports = [ ../cachix ./nix.nix ./shell.nix ];

  time.timeZone = "America/New_York";

  environment = {
    variables = {
      # WORKSHOP_DIR = config.workshop.path;
      EDITOR = "nvim";
      KERNEL_NAME = if pkgs.stdenv.isDarwin then "darwin" else "linux";
      LANG = "en_US.UTF-8";
      LC_ALL = "en_US.UTF-8";
      HOSTNAME = config.networking.hostName;
      TMPDIR = "/tmp";
    };

    shells = with pkgs; [ dash bashInteractive zsh ];

    systemPackages = with pkgs; [
      (python3.withPackages (ps: with ps; [ pip setuptools ]))
      (ripgrep.override { withPCRE2 = true; })

      binutils
      cachix
      coreutils
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
      tmux
      wget
      whois
      zsh
    ];
  };
}
