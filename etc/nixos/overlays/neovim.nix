final: prev: {
  neovim = prev.neovim.overrideAttrs (old: {
    postInstall = "wrapProgram $out/bin/nvim --set XDG_CONFIG_HOME $HOME/Workshop/etc";
  });
}
