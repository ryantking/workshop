{
  config,
  lib,
  pkgs,
  ...
}: {
  imports = [./fzf.nix ./starship.nix];

  programs = {
    zsh = {
      enable = true;
      enableAutosuggestions = true;
      enableCompletion = true;
      enableVteIntegration = false;
      autocd = true;
      defaultKeymap = "emacs";
      dirHashes = {
        workshop = "$WORKHOP_DIR";
        projects = "$HOME/Projects";
        dl = "$HOME/Downloads";
        docs = "$HOME/Documents";
        notes = "$HOME/Documents/notes";
        org = "$HOME/Nextcloud/Documents/org";
      };
      dotDir = ".config/zsh";
      envExtra = let
        vars = lib.concatStringsSep "\n" (lib.mapAttrsToList (n: v: ''export ${n}="${v}"'') config.shell.env);
      in ''
        has() {
          type "$1" >/dev/null 2>&1
        }

        ${vars}
      '';
      history.path = "$ZDOTDIR/.zsh_history";
      initExtra = ''
        ${config.shell.rcInit}

        typeset -aU path
      '';

      plugins = with pkgs; [
        {
          name = "fast-syntax-highlighting";
          src = "${zsh-fast-syntax-highlighting}/share/zsh/site-functions";
        }
        {
          name = "zsh-nix-shell";
          src = "${zsh-nix-shell}/share/zsh";
        }
        {
          name = "bd";
          src = "${zsh-bd}/share/zsh-bd";
        }
      ];
    };

    dircolors.enable = true;

    exa = {
      enable = true;
      enableAliases = true;
    };
  };

  home = {
    file.".dir_colors".source = "${pkgs.sources.nord-dircolors.src}/src/dir_colors";
    packages = [pkgs.zoxide];
  };

  shell.env.PATH = ["$XDG_BIN_HOME" "$PATH"];
}
