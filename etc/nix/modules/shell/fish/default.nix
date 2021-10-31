{ pkgs, lib, ... }:

{
  programs.fish.enable = true;
  environment.shells = [ pkgs.fish ];
  user.shell = pkgs.fish;

  users =
    lib.optionalAttrs pkgs.stdenvNoCC.isLinux { defaultUserShell = pkgs.fish; };

  hm.programs.fish = {
    enable = true;

    shellAbbrs = {
      g = "git";
      m = "make";
      t = "tmux";
      ta = "tmux a -t";
    };

    shellAliases = {
      cd = "z";
      cat = "bat --plain";
      tree = "exa -T";
      nvim = "nvim --startuptime /tmp/nvim-startuptime";
      ".." = "cd ..";
    };

    plugins = with pkgs.fishPlugins; [
      {
        name = "pure";
        src = pure.src;
      }
      {
        name = "fzf";
        src = fzf-fish.src;
      }
      {
        name = "forgit";
        src = forgit.src;
      }
    ];

    interactiveShellInit = builtins.readFile ./config.fish;
  };
}
