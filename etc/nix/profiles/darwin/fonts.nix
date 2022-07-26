{
  config,
  lib,
  pkgs,
  ...
}: {
  homebrew = {
    taps = ["homebrew/cask-fonts"];

    casks = ["font-sf-pro" "sf-symbols"];
  };

  fonts.fontDir.enable = false;
}
