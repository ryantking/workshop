{ config, ... }:

let
  fdCommand = "fd --follow --hidden --exclude .git";
  fzfOpts = with config.theme.colors;
  # "--no-bold --prompt='/ ' --pointer=• --marker=• --color " +
    "--no-bold --prompt='/ ' --color "
    + "fg:${fg2},bg:-1,hl:${secondary},fg+:${fg0},bg+:-1,hl+:${primary},"
    + "pointer:${alert},info:${bg3},spinner:${bg3},header:${bg3},prompt:${fg3},marker:${quinary}";
in {
  hm.programs.fzf = {
    enable = true;
    enableFishIntegration = true;

    defaultCommand = "${fdCommand}";
    defaultOptions = [ "${fzfOpts}" ];
    fileWidgetCommand = "${fdCommand} --type f";
    fileWidgetOptions = [ "${fzfOpts} --preview 'bat --color=always --plain {}'" ];
    changeDirWidgetCommand = "${fdCommand} --type d";
    changeDirWidgetOptions = [ "${fzfOpts} --preview 'exa -l --tree --level=2 --color=always {}'" ];
  };
}
