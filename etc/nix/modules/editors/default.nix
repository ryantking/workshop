{ config, pkgs, ... }:

{
  imports = [ ./neovim.nix ./emacs.nix ];

  config = {
    environment.variables = {
      EDITOR = "et";
      VISUAL_EDITOR = "emacs";
    };
  };
}
