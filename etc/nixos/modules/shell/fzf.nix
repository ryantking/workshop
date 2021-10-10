{ config, lib, ... }:

let
  inherit (lib) mkEnableOption mkIf;

  fdCommand = "fd --follow --hidden --exclude .git";
  fzfOpts = let c = config.modules.theme.colors; in
    "--no-bold --prompt='/ ' --pointer=• --marker=• --color " +
    "fg:${c.fg2},bg:-1,hl:${c.secondary},fg+:${c.fg0},bg+:-1,hl+:${c.primary}," +
    "pointer:${c.alert},info:${c.bg3},spinner:${c.bg3},header:${c.bg3},prompt:${c.fg3},marker:${c.quinary}";

  cfg = config.modules.shell.fzf;
in {
  options.modules.shell.fzf = {
    enable = mkEnableOption "command-line fuzzy finder";
  };

  config = mkIf cfg.enable {
    hm.programs.fzf = {
      enable = true;
      enableFishIntegration = config.modules.shell.fish.enable;

      defaultCommand = "${fdCommand}";
      defaultOptions = [ "${fzfOpts}" ];
      fileWidgetCommand = "${fdCommand} --type f";
      fileWidgetOptions = [ "${fzfOpts} --preview 'bat --color=always --plain {}'" ];
      changeDirWidgetCommand = "${fdCommand} --type d";
      changeDirWidgetOptions = [ "${fzfOpts} --preview 'exa -l --tree --level=2 --color=always {}'" ];
    };
  };
}
