{
  config,
  lib,
  pkgs,
  ...
}: {
  nix = {
    nixPath = ["darwin=/etc/nix/inputs/darwin"];
    trustedUsers = ["@admin" "@wheel"];
    useSandbox = false;
  };

  environment = {
    darwinConfig = "$WORKSHOP_DIR/lib/compat/darwin";
    systemPath = ["/usr/local/sbin" "/opt/homebrew/bin" "/opt/homebrew/sbin"];
    systemPackages = with pkgs; [m-cli mas terminal-notifier];
  };

  services = {
    activate-system.enable = true;
    nix-daemon.enable = true;
  };

  users.nix.configureBuildUsers = true;

  system.stateVersion = 4;

  homebrew = {
    enable = true;
    autoUpdate = true;
    global = {
      brewfile = true;
      noLock = true;
    };

    taps = ["homebrew/core" "homebrew/bundle" "homebrew/services"];
  };
}
