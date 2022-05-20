{ config
, lib
, pkgs
, ...
}: {
  nixpkgs.system = "x86_64-darwin";

  nix = {
    nixPath = [ "darwin=/etc/nix/inputs/darwin" ];
    trustedUsers = [ "@admin" ];
    useSandbox = false;
  };

  environment = {
    darwinConfig = "$WORKSHOP_DIR/lib/compat/darwin";
    variables.PATH = [ "/usr/local/sbin" "$PATH" ];
    systemPackages = with pkgs; [ m-cli mas terminal-notifier ];
  };

  services = {
    activate-system.enable = true;
    nix-daemon.enable = true;
  };

  users.nix.configureBuildUsers = true;

  system = {
    stateVersion = 4;
    activationScripts.postUserActivation.text = ''
      command -v brew > /dev/null ${pkgs.bash}/bin/bash -c "$(${pkgs.curl}/bin/curl -fsSL https://raw.githubusercontent.com/Homebrew/install/master/install.sh)"'';
  };

  homebrew = {
    enable = true;
    autoUpdate = true;
    global = {
      brewfile = true;
      noLock = true;
    };

    taps = [ "homebrew/bundle" "homebrew/core" "homebrew/services" ];
  };
}
