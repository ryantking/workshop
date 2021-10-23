{ config, inputs, lib, pkgs, ... }:

let
  inherit (builtins) readFile;
  inherit (lib) mkEnableOption mkIf;
  inherit (config.modules.theme) colors;

  cfg = config.modules.shell.tmux;
in {
  options.modules.shell.tmux = { enable = mkEnableOption "Tmux"; };

  config = mkIf cfg.enable {
    hm = {
      programs.tmux = {
        enable = true;
        baseIndex = 1;
        prefix = "C-a";
        resizeAmount = 10;
        shell = "${pkgs.fish}/bin/fish";
        shortcut = "a";
        terminal = "tmux-256color";

        plugins = with pkgs.tmuxPlugins; [
          sensible
          resurrect
          continuum
          yank
          open
          jump
          tmux-fzf
        ];

        extraConfig = readFile ./config.tmux + ''
          bind-key "'" run-shell -b "${pkgs.tmuxPlugins.tmux-fzf}/share/tmux-plugins/tmux-fzf/scripts/window.sh switch"
          bind-key '"' run-shell -b "${pkgs.tmuxPlugins.tmux-fzf}/share/tmux-plugins/tmux-fzf/scripts/session.sh attach"
        '';
      };
    };
  };
}
