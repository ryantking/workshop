{ inputs, config, pkgs, ... }:

{
  require = [ ./hardware-configuration.nix ];

  system.stateVersion = "21.05"; # Did you read the comment?

  boot = {
    loader = {
      systemd-boot = {
        enable = true;
        editor = false;
      };

      efi.canTouchEfiVariables = true;
    };

    cleanTmpDir = true;
  };

  nixpkgs.config.allowUnfree = true;
  nix = {
    package = pkgs.nixUnstable;
    extraOptions = ''
      experimental-features = nix-command flakes
    '';
   };

  time.timeZone = "America/New_York";

  networking = {
    hostName = "trashstation"; 
    useDHCP = false;
    interfaces.enp1s0.useDHCP = true;
  };

  i18n.defaultLocale = "en_US.UTF-8";
  console = {
    font = "Lat2-Terminus16";
    keyMap = "us";
  };

  security.sudo.wheelNeedsPassword = false;
  users = {
    mutableUsers = false;
    users.root.hashedPassword = "$6$xL0fzTOIJV5KEQ$clkH7gC8TThDI/2cqBmpi2eVDH5JTWXUMlPnh4Qwq3LhmB9tSwrlPlgF51V0lXBZtzyQnuQJX4.hM0pr2JcpV0";
  };

  user  = {
    name = "rking";
    uid = 1000;
    description = "Ryan King";
    home = "/home/rking";
    isNormalUser = true;
    useDefaultShell = true;
    extraGroups = [ "wheel" "networkmanager" ];
    hashedPassword = "$6$xL0fzTOIJV5KEQ$clkH7gC8TThDI/2cqBmpi2eVDH5JTWXUMlPnh4Qwq3LhmB9tSwrlPlgF51V0lXBZtzyQnuQJX4.hM0pr2JcpV0";
  };

  environment.systemPackages = with pkgs; [
    file
    git
    neovim
    psmisc
    rclone
    unzip
    wget

    binutils
    coreutils
    pciutils
    usbutils
  ];

  imports = [ inputs.home-manager.nixosModules.home-manager ];

  hm.home.stateVersion = "21.05";
  home-manager = {
    useUserPackages = true;
    useGlobalPkgs = true;
  };
  hm.programs.home-manager.enable = true;
  hm.systemd.user.startServices = true;

  modules = {
    hardware = {
      cpu.vendor = "intel";
      video.gpu = "nvidia";
      audio.enable = true;
      bluetooth.enable = true;
    };

    desktop = {
      sddm.enable = true;
      plasma.enable = true;

      term = {
        default = "alacritty";
        alacritty.enable = true;
      };
    };

    editors = {
      default = "neovim";
      neovim.enable = true;
    };

    shell = {
      bat.enable = true;
      bottom.enable = true;
      broot.enable = true;
      direnv.enable = true;
      exa.enable = true;
      fish.enable = true;
      fzf.enable = true;
      jq.enable = true;
      nnn.enable = true;
      tmux.enable = true;
      zoxide.enable = true;

      git = {
        enable = true;
        gh.enable = true;
      };

      gnupg = {
        enable = true;
        sshAgent = true;
        cacheTTL = 28800;
      };
    };

    theme = {
      colorscheme = "nord";

      fonts = rec {
        sans = {
          family = "Roboto";
          style = "Light";
          size = 12;
          pkg = pkgs.roboto;
        };
        serif = {
          family = "Roboto Slab";
          style = "Light";
          size = 12;
          pkg = pkgs.roboto-slab;
        };
        mono = {
          family = "RobotoMono Nerd Font";
          style = "Light";
          size = 12;
          pkg = pkgs.nerdfonts.override;
        };
        ui = sans;
        term = {
          family = "scientifica";
          style = "Medium";
          size = 11;
          pkg = pkgs.scientifica;
        };
      };
    };
  };
}
