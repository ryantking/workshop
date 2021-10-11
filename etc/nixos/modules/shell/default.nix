{ config, lib, pkgs, ... }:

let
  inherit (lib) mkEnableOption mkIf;

  cfg = config.modules.shell;
in
{
  options.modules.shell = {
    bat.enable = mkEnableOption "bat";
    bottom.enable = mkEnableOption "bottom";
    broot.enable = mkEnableOption "broot";
    direnv.enable = mkEnableOption "direnv";
    exa.enable = mkEnableOption "exa (modern ls replacement)";
    jq.enable = mkEnableOption "jq";
    nnn.enable = mkEnableOption "nnn";
    zoxide.enable = mkEnableOption "zoxide";
  };

  config.hm = {
    programs = {
      bat = mkIf cfg.bat.enable {
        enable = true;
        config = { pager = "less -FR"; };
      };

      bottom = mkIf cfg.bottom.enable {
        enable = true;
      };

      broot = mkIf cfg.broot.enable {
        enable = true;
        enableFishIntegration = config.modules.shell.fish.enable;
      };

      direnv = mkIf cfg.direnv.enable {
        enable = true;
        nix-direnv.enable = true;
        enableFishIntegration = config.modules.shell.fish.enable;

        stdlib = ''
          # Store .direnv in cache instead of project dir
          declare -A direnv_layout_dirs
          direnv_layout_dir() {
            echo "''${direnv_layout_dirs[$PWD]:=$(
              echo -n "$XDG_CACHE_HOME"/direnv/layouts/
              echo -n "$PWD" | shasum | cut -d ' ' -f 1
            )}"
          }
        '';
      };

      exa = mkIf cfg.exa.enable {
        enable = true;
        enableAliases = true;
      };

      jq = mkIf cfg.jq.enable {
        enable = true;
      };

      nnn = mkIf cfg.nnn.enable {
        enable = true;
      };

      zoxide = mkIf cfg.zoxide.enable {
        enable = true;
        enableFishIntegration = config.modules.shell.fish.enable;
      };
    };

    home = {
      packages = with pkgs; [
        fd
        iotop
        most
        tealdeer
        ripgrep
        ripgrep-all
      ];

      sessionVariables = {
        PAGER = "most -s -w";
      };

      activation = mkIf cfg.bat.enable {
        "batCacheBuild" = {
          before = [ ];
          after = [ "linkGeneration" ];
          data = "${pkgs.bat}/bin/bat cache --build";
        };
      };
    };
  };
}
