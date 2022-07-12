{
  config,
  lib,
  pkgs,
  inputs,
  ...
}: let
  inherit (config.colorscheme) colors;

  fzfTheme =
    lib.concatStringsSep ","
    (lib.mapAttrsToList (n: v: "${n}:#${v}")
      {
        "bg" = colors.base00;
        "bg+" = colors.base00;
        "fg" = colors.base04;
        "fg+" = colors.base06;
        "header" = colors.base0D;
        "hl" = colors.base0D;
        "hl+" = colors.base0D;
        "info" = colors.base0A;
        "marker" = colors.base0C;
        "pointer" = colors.base0C;
        "prompt" = colors.base0A;
        "spinner" = colors.base0C;
      });

  fdCmd = "${pkgs.fd}/bin/fd --follow --hidden --exclude .git 2>/dev/null";
  fzfOpts = ''--no-bold --prompt='/ ' --pointer='➜' --marker='·' --color=\"${fzfTheme}\"'';
in {
  home.packages = [pkgs.fzf];

  shell.env = {
    FZF_DEFAULT_COMMAND = "${fdCmd}";
    FZF_DEFAULT_OPTS = "${fzfOpts}";
    FZF_CTRL_T_COMMAND = "${fdCmd}";
    FZF_CTRL_T_OPTS = "${fzfOpts} --preview 'bat --color=always --plain {}'";
    FZF_ALT_C_COMMAND = "${fdCmd} --type d";
    FZF_ALT_C_OPTS = "${fzfOpts} --preview 'exa -l --tree --level=2 --color=always {}'";
  };
}
