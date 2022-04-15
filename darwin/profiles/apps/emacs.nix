{ config, pkgs, ... }:

{
  homebrew = {
    taps = [ "railwaycat/emacsmacport" ];
    brews = [ "pngpaste" ];
    extraConfig = ''
      brew "emacs-mac", args: [
        "with-nativ-comp", "with-mac-metal", "with-imagemagick", "with-rsvg",
        "with-natural-title-bar", "with-emacs-big-sur-icon", "with-starter"
      ]'';
  };
}
