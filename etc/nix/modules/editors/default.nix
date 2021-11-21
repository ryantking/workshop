{ config, pkgs, ... }:

{
  imports = [ ./neovim ];

  config = {
    environment.variables.EDITOR = "nvim";

    # hm.programs.emacs = {
    #   enable = true;
    #   package = pkgs.emacsGcc;
    # };

    # services.emacs = {
    #   enable = true;
    #   package = pkgs.emacsGcc;
    # };
  };
}
