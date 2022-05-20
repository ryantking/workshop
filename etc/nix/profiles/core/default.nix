{ config
, lib
, pkgs
, ...
}: {
  imports = [ ./cachix.nix ./nix.nix ./shell.nix ];

  environment = {
    variables = {
      WORKSHOP_DIR = config.workshop.home;
      EDITOR = "vi -e";
      VISUAL = "vim";
      HOSTNAME = config.networking.hostName;
      KERNEL_NAME =
        if pkgs.stdenv.isDarwin
        then "darwin"
        else "linux";
      LANG = "en_US.UTF-8";
      LC_ALL = "en_US.UTF-8";
      CACHEDIR = "$HOME/.cache";
      TMPDIR = "/tmp";
      XDG_BIN_HOME = "$HOME/.local/bin";
      XDG_CACHE_HOME = "$HOME/.cache";
      XDG_CONFIG_HOME = "$HOME/.config";
      XDG_DATA_HOME = "$HOME/.local/share";
      XDG_RUNTIME_DIR = "/tmp";
      XDG_STATE_HOME = "$HOME/.local/state";
      ZDOTDIR = "$HOME/.config/zsh";
    };

    systemPackages = with pkgs; [
      (python3.withPackages (ps: with ps; [ pip setuptools ]))
      (ripgrep.override { withPCRE2 = true; })

      bashInteractive
      cachix
      coreutils
      curl
      dash
      dnsutils
      findutils
      gawk
      gnumake
      gnused
      grc
      less
      lua
      manix
      more
      moreutils
      nix-index
      nix-tree
      nmap
      nvfetcher
      openssh
      openssl
      rsync
      skim
      wget
      whois
      vim
    ];
  };
}
