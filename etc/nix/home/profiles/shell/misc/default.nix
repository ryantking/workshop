{ config, ... }:

{
  programs = {
    bottom = { enable = true; };
    broot = { enable = true; };
    jq = { enable = true; };
    nnn = { enable = true; };
    zoxide = { enable = true; };

    exa = {
      enable = true;
      enableAliases = true;
    };
  };
}
