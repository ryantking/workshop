{
  config,
  lib,
  pkgs,
  ...
}: {
  home.activation."ensureTealdeerCache" = "mkdir -p ${config.xdg.cacheHome}/tealdeer";

  shell.env = {
    TEALDEER_CONFIG_DIR = "$XDG_CONFIG_HOME/tealdeer";
    TEALDEER_CACHE_DIR = "$XDG_CACHE_HOME/tealdeer";
  };

  xdg.configFile."tealdeer/config.toml".text = ''
    # https://dbrgn.github.io/tealdeer/config.html
    [display]
    use_pager = false
    compact = false
    [updates]
    auto_update = true
    auto_update_interval_hours = 24
  '';
}
