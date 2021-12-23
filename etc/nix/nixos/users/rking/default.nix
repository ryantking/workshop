{ hmUsers, config, lib, pkgs, ... }:

let
  inherit (builtins) baseNamefOf;
  inherit (lib) optionalAttrs;
  inherit (pkgs.stdenv) isDarwin isLinux;

  user = baseNameOf ./.;
in {
  environment.etc."mytest.conf".text = config.nix.extraOptions;

  home-manager = {
    useGlobalPkgs = true;
    useUserPackages = true;
    users = { inherit (hmUsers) rking; };
  };

  security = (optionalAttrs isLinux { sudo.wheelNeedsPassword = false; });

  users.users.rking = {
    shell = pkgs.zsh;
  } // (optionalAttrs (isLinux) {
    uid = 1000;
    isNormalUser = true;
    extraGroups = [ "wheel" "networkmanager" "audio" "pulse" ];
    home = "/home/${user}";
    hashedPassword =
      "$6$xL0fzTOIJV5KEQ$clkH7gC8TThDI/2cqBmpi2eVDH5JTWXUMlPnh4Qwq3LhmB9tSwrlPlgF51V0lXBZtzyQnuQJX4.hM0pr2JcpV0";
    openssh.authorizedKeys.keys = [
      "ssh-rsa AAAAB3NzaC1yc2EAAAADAQABAAACAQChmn6rG77RylvNxSDCuLX8IO6loqV7gNdsgqjq6ModHQshiVb1yG/zNvstS2fLWRdnKvQnhPj05xmQT0GNmL74+04hsQA2W2SJ6qECJ3QdvwztlB3d8QS2TXg9zmh1RoQRJ/aUKXEz0JV53V4SHcTgsUvApYcWXkNSbTxMl56BAOLr7xmHP1G6KBXD7rawJIrqpgtXmcxMaEelKGwp1Sj/byIZ+SrdiIfBGGR8R0aCNdyWi98YMFpTupzUjOmQjqTTG76DI6DVvwR1YTbYmoJ7BiWrmTr3ene2j7hfUmNGNpgm+psu1/yzg31yRLacfgYYwnZGQXo0oIFVcyJdLcVUfadtDxDfBcsWDCUMW2pEkTOUxWqjfns07RncOxhXyN6B/FgLdGK95KXygsrU907pYfwq/sxq1//1KQcl/yyCRNoEA/SHbwjB/wjI2BHgSjxnKtyYwP4HFL+JuLPfbtTigomPhFOE0fIUgasUqQrBQ0XTnuT2k4YM+vJRTgOHaYfRrgABzNDij5S/8RDlsZQFq+rKTdtT368Wp1Vu/m7sLAh1Ig6cBPenexgabgSaopNWA2fKA8GR+smNsFn5thMy5pYmthBRGi35F9pyaRZ/w158s9GQ5jgz8UFMu611RFrX+ZPq9CX3/ZV/Z+jAIKDvyDSn0O8Vxi9o6gTyG7vXzQ== (none)"
    ];
  }) // (optionalAttrs (isDarwin) { home = "/Users/${user}"; });

  # environment.variables = let inherit (settings) xdg;
  # in {
  #   CACHEDIR = xdg.cache;
  #   XDG_BIN_HOME = xdg.bin;
  #   XDG_CACHE_HOME = xdg.cache;
  #   XDG_CONFIG_HOME = xdg.config;
  #   XDG_DATA_HOME = xdg.data;
  #   XDG_RUNTIME_DIR = "/tmp";
  #   XDG_STATE_HOME = xdg.state;
  # };
  # settings = {
  #
  #   env = {
  #     GITHUB_USER = settings.githubUsername;
  #   };
  #
  #   hm = {
  #     home = {
  #       stateVersion = if isDarwin then "21.11" else config.system.stateVersion;
  #       username = settings.username;
  #     };
  #
  #     xdg = { enable = true; };
  #   };
  # };
}
