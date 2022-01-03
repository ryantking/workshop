{ config, lib, pkgs, ... }:

{
  environment = {
    shellInit = ''
      export STARSHIP_CONFIG=${pkgs.writeText "starship.toml" (lib.fileContents ./starship.toml)}
    '';

    # shellAliases.nixos-option = "nixos-option -I nixpkgs=${self}/lib/compat";
  };

  programs.bash = {
    # promptInit = ''
    #   eval "$(${pkgs.starship}/bin/starship init bash)"
    # '';
    interactiveShellInit = ''
      eval "$(${pkgs.direnv}/bin/direnv hook bash)"
    '';
  };
}
