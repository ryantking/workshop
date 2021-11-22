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

    home = {
      activation.updateFisher = ''
        $DRY_RUN_CMD cat > $HOME/.config/fish/fish_plugins << EOF
        jorgebucaran/fisher
        jhillyerd/plugin-git
        PatrickF1/fzf.fish
        pure-fish/pure
        wfxr/forgit

        oh-my-fish/plugin-brew
        oh-my-fish/plugin-foreign-env
        oh-my-fish/plugin-grc
        oh-my-fish/plugin-osx
        oh-my-fish/plugin-rustup
        oh-my-fish/plugin-thefuck
        EOF
        $DRY_RUN_CMD fish -c "fisher update | rg --color=never 'Installing|Updating|Removing|Updated'"
      '';
    };
  };
}
