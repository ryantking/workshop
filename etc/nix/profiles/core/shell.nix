{ self
, config
, lib
, pkgs
, ...
}:
let
  fmt = pkgs.formats.toml { };
  starshipSettings = fmt.generate "starship.toml" config.shell.prompt.starship;
in
{
  environment = {
    shellAliases =
      if pkgs.stdenv.isDarwin
      then {
        darwin-option = "darwin-option -I nixpkgs=${self}/lib/compat";
      }
      else {
        nixos-option = "nixos-option -I nixpkgs=${self}/lib/compat";
      };

    shells = with pkgs; [ bashInteractive dash zsh ];
  };

  programs = {
    bash.interactiveShellInit = ''
      if [[ $TERM != "dumb" && (-z $INSIDE_EMACS || $INSIDE_EMACS == "vterm") ]]; then
         export STARSHIP_CONFIG=${starshipSettings}
         eval "$(${pkgs.starship}/bin/starship init bash)"
      fi
    '';

    zsh.enable = true;
  };
}
