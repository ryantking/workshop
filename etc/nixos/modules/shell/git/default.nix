{ config, lib, pkgs, options, ... }:

let
  inherit (lib) mkEnableOption mkMerge mkOption mkIf types;

  cfg = config.modules.shell.git;
in {
  options.modules.shell.git = {
    enable = mkEnableOption "Git version control";

    userName = mkOption {
      default = "Ryan King";
      type = types.str;
    };

    userEmail = mkOption {
      default = "ryan.taylor.king@gmail.com";
      type = types.str;
    };

    delta = mkOption {
      description = "Use delta for diffs";
      default = true;
      type = types.bool;
    };

    gh.enable = mkEnableOption "GitHub CLI tool";
  };

  config = mkIf cfg.enable (mkMerge [
    {
      hm.programs.git = import ./config.nix { inherit cfg; pkg = pkgs.gitAndTools.gitFull; };

      hm.home.packages = with pkgs; [
        git-up
        git-my
        git-sync
        git-open
        git-trim
        git-crypt
        git-subrepo
        git-standup
      ];
    }

    (mkIf cfg.delta {
      hm.home.sessionVariables = {
        DELTA_PAGER = "less -FR"; 
      };

      hm.programs.git.delta = {
        enable = true;

        options = {
          line-numbers = true;
          features = "decorations";
          decorations = {
            file-style = "bold white ul";
            file-decoration-stlye = "none";
            whitespace-error-style = "22 reverse";
          };
        };
      };
    })

    (mkIf cfg.gh.enable {
      hm = {
        programs.gh = {
          enable = true;

          aliases = {
            co = "pr checkout";
            cl = "repo clone";
          };

          gitProtocol = "ssh";
        };

        home.file.".config/gh/hosts.yml".source = ./hosts.yml;
      };
    })
  ]);
}
