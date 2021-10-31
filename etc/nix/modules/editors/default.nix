{ config, ... }:

{
  imports = [ ./neovim ];

  config = { environment.variables.EDITOR = "nvim"; };
}
