{ self
, common
, digga
, colors
, ...
}:

{
  imports = [ (digga.lib.importExportableModules ./modules) ];

  modules = [ colors.homeManagerModule ];

  importables = rec {
    profiles = digga.lib.rakeLeaves ./profiles;
    suites = with profiles; rec {
      base = [ git gpg secrets ssh themes.nord zsh ];
      gui = [ alacritty kitty ];
      devel = [ bat direnv emacs languages.go mail tealdeer ];
    };
  };

  users = {
    rking = { suites, ... }: {
      imports = suites.base ++ suites.gui ++ suites.devel;
    };
  };
}
