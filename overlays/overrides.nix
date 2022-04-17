channels: final: prev: {
  __dontExport = true;
  inherit (channels.nixpkgs-latest) alejandra go_1_18 tmux-fzf;
}
