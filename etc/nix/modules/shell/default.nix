{ pkgs, ... }:

{
  imports = [ ./fish ./git ./tmux ./fzf.nix ./gnupg.nix ];

  config = {
    hm = {
      programs = {
        bottom = { enable = true; };
        jq = { enable = true; };
        nnn = { enable = true; };

        bat = {
          enable = true;
          config = { pager = "less -FR"; };
        };

        broot = {
          enable = true;
          enableFishIntegration = true;
        };

        direnv = {
          enable = true;
          nix-direnv.enable = true;
          enableFishIntegration = true;

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

        exa = {
          enable = true;
          enableAliases = true;
        };

        zoxide = {
          enable = true;
          enableFishIntegration = true;
        };
      };

      home = {
        packages = with pkgs; [
          fd
          gnugrep
          grc
          most
          procs
          (ripgrep.override { withPCRE2 = true; })
          ripgrep-all
          sd
          tealdeer
        ];

        sessionVariables = { PAGER = "most -s -w"; };

        activation = {
          "batCacheBuild" = {
            before = [ ];
            after = [ "linkGeneration" ];
            data = "${pkgs.bat}/bin/bat cache --build";
          };
        };
      };
    };
  };
}
