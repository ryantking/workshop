{ config, inputs, lib, options, pkgs, ... }:

let
  inherit (builtins) readFile;
  inherit (lib) mkEnableOption mkOption mkIf types;

  cfg = config.modules.shell.fish;
in {
  options.modules.shell.fish = { enable = mkEnableOption "Fish shell"; };

  config = {
    users.defaultUserShell = pkgs.fish;

    programs = {
      fish.enable = true;
    };

    hm.programs = {
      fish = {
        enable = true;

        shellAbbrs = {
          g = "git";
          m = "make";
          t = "tmux";
          ta = "tmux a -t";
        };

        shellAliases = {
          cp = "z";
          cat = "bat --plain";
          tree = "exa -T";
          ".." = "cd ..";
        };

        plugins = with pkgs.fishPlugins; [
          { name = "pure"; src = pure.src; }
          { name = "fzf"; src = fzf-fish.src; }
          { name = "forgit"; src = forgit.src; }
        ];

        interactiveShellInit = readFile ./config.fish;
      };
    };
  };
}
