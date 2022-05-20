{ self
, config
, lib
, pkgs
, ...
}:
let
  inherit (lib) optionals;
  inherit (pkgs.stdenv) isLinux;
  inherit (lib) concatStrings;
in
{
  programs.starship = {
    enable = true;
    settings = config.shell.prompt.starship;
  };

  shell.prompt.starship = {
    format = concatStrings [
      "$username"
      "$hostname"
      "$directory"
      "$git_branch"
      "$git_state"
      "$git_status"
      "$cmd_duration"
      "$jobs"
      "$line_break"
      "$python"
      "$nix_shell"
      "$character"
    ];

    directory = {
      style = "bold blue";
      read_only = "";
    };

    git_branch = {
      format = "[$branch]($style)";
      style = "bright-black";
    };

    git_status = {
      format = "[[(*$conflicted$untracked$modified$staged$renamed$deleted)](218) ($ahead_behind$stashed)]($style) ";
      style = "cyan";
      conflicted = "​";
      untracked = "​";
      modified = "​";
      staged = "​";
      renamed = "​";
      deleted = "​";
      stashed = "≡";
    };

    git_state = {
      format = "([$state( $progress_current/$progress_total)]($style)) ";
      style = "bright-black";
    };

    cmd_duration = {
      format = "[$duration]($style) ";
      style = "yellow";
    };

    python = {
      format = "[$virtualenv]($style) ";
      style = "bright-black";
    };

    nix_shell = {
      symbol = "️❆ ";
      format = "[$symbol](bold blue)[$name]($style) ";
      style = "bright-black";
    };
  };
}
