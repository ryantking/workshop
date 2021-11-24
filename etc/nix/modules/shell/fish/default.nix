{ pkgs, lib, ... }:

{
  programs.fish.enable = true;
  environment.shells = [ pkgs.fish ];
  user.shell = pkgs.fish;

  users = lib.optionalAttrs pkgs.stdenv.isLinux { defaultUserShell = pkgs.fish; };

  hm = {
    programs.fish = {
      enable = true;

      shellAbbrs = {
        g = "git";
        m = "make";
        t = "tmux";
      };

      shellAliases = {
        cat = "bat --plain";
        tree = "exa -T";
        nvim = "nvim --startuptime /tmp/nvim-startuptime";
        ".." = "cd ..";
        ta = "tmux a";
        tat = "tmux a -t";
      };

      interactiveShellInit = builtins.readFile ./config.fish;
    };

    home.file.".config/fish/install_plugins.fish" = {
      source = ./install_plugins.fish;
      onChange = "$DRY_RUN_CMD fish $HOME/.config/fish/install_plugins.fish";
    };
  };
}
