final: prev: {
  yabai = prev.yabai.overrideAttrs (old: rec {
    version = "3.3.10";
    src = builtins.fetchTarball {
      url =
        "https://github.com/koekeishiya/yabai/releases/download/v${version}/yabai-v${version}.tar.gz";
      sha256 = "sha256:1z95njalhvyfs2xx6d91p9b013pc4ad846drhw0k5gipvl03pp92";
    };
  });
}
