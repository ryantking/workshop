{ self, config, lib, pkgs, ... }:

let
  inherit (pkgs.stdenv) isDarwin;

  fmt = pkgs.formats.toml { };
  starshipSettings = fmt.generate "starship.toml" config.shell.prompt.starship;
in
{
  environment.shellAliases = (if isDarwin then {
    darwin-option = "darwin-option -I nixpkgs=${self}/lib/compat";
  } else {
    nixos-option = "nixos-option -I nixpkgs=${self}/lib/compat";
  });

  programs = {
    bash.interactiveShellInit = ''
      eval $(${pkgs.direnv}/bin/direnv hook bash)

      if [[ $TERM != "dumb" && (-z $INSIDE_EMACS || $INSIDE_EMACS == "vterm") ]]; then
         export STARSHIP_CONFIG=${starshipSettings}
         eval "$(${pkgs.starship}/bin/starship init bash)"
      fi
    '';

    zsh = {
      enable = true;
      enableCompletion = true;
      enableBashCompletion = true;
      promptInit = ''
        if [[ $TERM != "dumb" && (-z $INSIDE_EMACS || $INSIDE_EMACS == "vterm") ]]; then
            export STARSHIP_CONFIG=${starshipSettings}
            eval "$(${pkgs.starship}/bin/starship init zsh)"
        fi
      '';
    };
  };
}
