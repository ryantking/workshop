{ config, pkgs, ... }:

{
  imports = [ ./neovim.nix ./emacs ];

  config = {
    environment.variables = {
      EDITOR = "et";
      VISUAL_EDITOR = "emacs";
    };
  };
}
