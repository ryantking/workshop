{ config, lib, pkgs, inputs, ... }:

let
  inherit (lib) concatStringsSep mapAttrsToList;

  fzfTheme =
    with config.colorscheme.colors;
    concatStringsSep ","
      (mapAttrsToList (n: v: "${n}:#${v}")
        {
          "bg" = base00;
          "bg+" = base00;
          "fg" = base04;
          "fg+" = base06;
          "header" = base0D;
          "hl" = base0D;
          "hl+" = base0D;
          "info" = base0A;
          "marker" = base0C;
          "pointer" = base0C;
          "prompt" = base0A;
          "spinner" = base0C;
        });

  fdCmd = "${pkgs.fd}/bin/fd --follow --hidden --exclude .git 2>/dev/null";
  fzfOpts = ''--no-bold --prompt='/ ' --pointer='➜' --marker='·' --color=\"${fzfTheme}\"'';
in
{
  home.packages = [ pkgs.fzf ];

  shell.env = {
    FZF_DEFAULT_COMMAND = "${fdCmd}";
    FZF_DEFAULT_OPTS = "${fzfOpts}";
    FZF_CTRL_T_COMMAND = "${fdCmd}";
    FZF_CTRL_T_OPTS = "${fzfOpts} --preview 'bat --color=always --plain {}'";
    FZF_ALT_C_COMMAND = "${fdCmd} --type d";
    FZF_ALT_C_OPTS = "${fzfOpts} --preview 'exa -l --tree --level=2 --color=always {}'";
  };
}
