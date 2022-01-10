{ self, config, lib, pkgs, ... }:

let
  inherit (lib) mkMerge optionalAttrs;
  inherit (pkgs.stdenv) isLinux isDarwin;
in
{
  environment = mkMerge [
    {
      shellInit = ''
        export STARSHIP_CONFIG=${pkgs.writeText "starship.toml" (lib.fileContents ./starship.toml)}
      '';
    }
    (optionalAttrs isLinux {
      shellAliases.nixos-option = "nixos-option -I nixpkgs=${self}/lib/compat";
    })
  ];

  programs = {
    bash = mkMerge [
      {
        interactiveShellInit = ''
          eval "$(${pkgs.direnv}/bin/direnv hook bash)"
        '' + (if isDarwin then ''eval "$(${pkgs.starship}/bin/starship init bash)"'' else "");
      }
      (optionalAttrs isLinux { promptInit = ''eval "$(${pkgs.starship}/bin/starship init bash)"''; })
    ];

    zsh = {
      enable = true;
      enableCompletion = false;
      enableBashCompletion = false;
      promptInit = "";
    };
  };
}
