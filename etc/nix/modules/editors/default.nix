{ config, pkgs, ... }:

{
  imports = [ ./neovim ];

  config = { environment.variables.EDITOR = "nvim"; };
}
