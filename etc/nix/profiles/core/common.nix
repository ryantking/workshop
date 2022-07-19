{
  config,
  lib,
  pkgs,
  ...
}: {
  imports = [./cachix.nix];

  time.timeZone = "America/New_York";

  nix = {
    package = pkgs.nixUnstable;
    gc.automatic = true;
    useSandbox = lib.mkDefault (!pkgs.stdenv.hostPlatform.isDarwin);
    allowedUsers = ["*"];
    trustedUsers = ["root" "@wheel"];

    linkInputs = true;
    generateRegistryFromInputs = true;
    generateNixPathFromInputs = true;
  };

  home-manager = {
    useGlobalPkgs = true;
    useUserPackages = true;
  };

  programs = {
    bash.interactiveShellInit = let
      fmt = pkgs.formats.toml {};
      starshipSettings = fmt.generate "starship.toml" config.shell.prompt.starship;
    in ''
      if [[ $TERM != "dumb" && (-z $INSIDE_EMACS || $INSIDE_EMACS == "vterm") ]]; then
         export STARSHIP_CONFIG=${starshipSettings}
         eval "$(${pkgs.starship}/bin/starship init bash)"
      fi
    '';

    zsh = {
      enable = true;
      enableCompletion = true;
    };
  };

  environment = {
    shells = [
      pkgs.bashInteractive
      pkgs.zsh
    ];

    pathsToLink = ["/share/zsh"];

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
      (ripgrep.override {withPCRE2 = true;})

      bashInteractive
      bat
      cachix
      coreutils
      curl
      dash
      dnsutils
      findutils
      gawk
      gnumake
      gnused
      gnutar
      grc
      jq
      less
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
