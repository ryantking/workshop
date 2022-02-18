{ inputs, ... }:

with inputs;

let
  inherit (digga.lib) rakeLeaves importExportableModules;
in
{
  imports = [ (importExportableModules ./modules) ];

  modules = [ colors.homeManagerModule ];

  importables = rec {
    profiles = rakeLeaves ./profiles;
    suites = with profiles; rec {
      base = [ alacritty bat direnv git gpg languages.go tealdeer zsh emacs ssh ];
    };
  };

  users = {
    rking = { suites, ... }: { imports = suites.base; };
  };
}
