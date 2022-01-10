{ inputs, config, options, lib, pkgs, ... }:

let
  inherit (builtins) elem getEnv baseNamefOf;
  inherit (inputs) colors;
  inherit (lib) optionalAttrs mkAliasDefinitions;
  inherit (pkgs.stdenv) isDarwin isLinux;
  inherit (config) my;

  name =
    let envName = getEnv "USER";
    in if elem envName [ "" "root" ] then my.username else envName;
  theme = let envTheme = getEnv "THEME"; in if envTheme != "" then envTheme else "nord";
in
{
  users.users.${my.username} = mkAliasDefinitions options.my.user;

  colorscheme = colors.colorSchemes.${theme};

  my.user = {
    inherit name;
    shell = pkgs.zsh;
    packages = import ./packages.nix { inherit pkgs; };
  } // (optionalAttrs (isLinux) {
    uid = 1000;
    isNormalUser = true;
    extraGroups = [ "wheel" "networkmanager" "audio" "pulse" ];
    home = "/home/${name}";
    hashedPassword =
      "$6$xL0fzTOIJV5KEQ$clkH7gC8TThDI/2cqBmpi2eVDH5JTWXUMlPnh4Qwq3LhmB9tSwrlPlgF51V0lXBZtzyQnuQJX4.hM0pr2JcpV0";
    openssh.authorizedKeys.keys = [
      "ssh-rsa AAAAB3NzaC1yc2EAAAADAQABAAACAQChmn6rG77RylvNxSDCuLX8IO6loqV7gNdsgqjq6ModHQshiVb1yG/zNvstS2fLWRdnKvQnhPj05xmQT0GNmL74+04hsQA2W2SJ6qECJ3QdvwztlB3d8QS2TXg9zmh1RoQRJ/aUKXEz0JV53V4SHcTgsUvApYcWXkNSbTxMl56BAOLr7xmHP1G6KBXD7rawJIrqpgtXmcxMaEelKGwp1Sj/byIZ+SrdiIfBGGR8R0aCNdyWi98YMFpTupzUjOmQjqTTG76DI6DVvwR1YTbYmoJ7BiWrmTr3ene2j7hfUmNGNpgm+psu1/yzg31yRLacfgYYwnZGQXo0oIFVcyJdLcVUfadtDxDfBcsWDCUMW2pEkTOUxWqjfns07RncOxhXyN6B/FgLdGK95KXygsrU907pYfwq/sxq1//1KQcl/yyCRNoEA/SHbwjB/wjI2BHgSjxnKtyYwP4HFL+JuLPfbtTigomPhFOE0fIUgasUqQrBQ0XTnuT2k4YM+vJRTgOHaYfRrgABzNDij5S/8RDlsZQFq+rKTdtT368Wp1Vu/m7sLAh1Ig6cBPenexgabgSaopNWA2fKA8GR+smNsFn5thMy5pYmthBRGi35F9pyaRZ/w158s9GQ5jgz8UFMu611RFrX+ZPq9CX3/ZV/Z+jAIKDvyDSn0O8Vxi9o6gTyG7vXzQ== (none)"
    ];
  }) // (optionalAttrs (isDarwin) { home = "/Users/${name}"; });

  environment.variables =
    let inherit (my) xdg;
    in
    {
      CACHEDIR = xdg.cache;
      XDG_BIN_HOME = xdg.bin;
      XDG_CACHE_HOME = xdg.cache;
      XDG_CONFIG_HOME = xdg.config;
      XDG_DATA_HOME = xdg.data;
      XDG_RUNTIME_DIR = "/tmp";
      XDG_STATE_HOME = xdg.state;
    };

  home-manager = {
    useGlobalPkgs = true;
    useUserPackages = true;
    users.${my.username} = mkAliasDefinitions options.my.hm;
  };

  security = (optionalAttrs isLinux { sudo.wheelNeedsPassword = false; });

  my = {
    env = { GITHUB_USER = my.githubUsername; };

    hm = {
      home = {
        stateVersion = if isDarwin then "21.11" else config.system.stateVersion;
        username = my.username;
      };

      xdg = { enable = true; };
    };
  };
}
