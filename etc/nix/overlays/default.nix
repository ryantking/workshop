{ inputs, pkgs, nixpkgs, ... }:

{
  nixpkgs.overlays = [
    inputs.neovim-nightly-overlay.overlay
    inputs.emacs-overlay.overlay
    (final: prev: rec {
      # emacsPgtkGcc = let
      #   emacsDir = "$HOME/.config/emacs"; 
      # in (prev.emacsPgtkGcc.overrideAttrs (o: {
      #   postInstall = o.postInstall or "" + ''
      #     if [[ -d ${emacsDir} ]]; then
      #       ${emacsDir}/bin/doom -y upgrade
      #     else
      #       git clone --depth 1 https://github.com/hlissner/doom-emacs ${emacsDir}
      #       ${emacsDir}/bin/doom -y install
      #     fi
      #   '';
      # }));

      fennel = prev.fennel.overrideAttrs (_: {
        src = prev.fetchFromGitHub {
          owner = "bakpakin";
          repo = "Fennel";
          rev = "6ba3c845c98d7371c5bb6869a08f4698960573fe";
          sha256 = "sha256-97NfFaq4Xz1mwzcAbYDT98Eyd1RJRd41gIj+64x3teE=";
        };
      });

      python39 = let lib = prev.lib;
      in prev.python39.override {
        packageOverrides = final: prev: {
          beautifulsoup4 = prev.beautifulsoup4.overrideAttrs
          (old: { propagatedBuildInputs = lib.remove prev.lxml old.propagatedBuildInputs; });
        };
      };

      nodePackages = prev.nodePackages // (import ./node-packages { pkgs = prev; });

      python39Packages = python39.pkgs;

      skhd = prev.skhd.overrideAttrs (o: rec {
        buildInputs = o.buildInputs ++ [ final.makeWrapper ];
        postInstall = o.postInstall or "" + ''
          wrapProgram "$out/bin/skhd" --set SHELL /bin/bash
        '';
      });

      tmuxPlugins = prev.tmuxPlugins // {
        tmux-fzf = prev.tmuxPlugins.tmux-fzf.overrideAttrs (_: {
          src = prev.fetchFromGitHub {
            owner = "sainnhe";
            repo = "tmux-fzf";
            rev = "8bba6a429d3ec808031f972384f0bd28b0ce38c1";
            sha256 = "sha256-QbBqv1cFSuGR97layXoeBpd5MP6PGlRhmLbRd6KYh6Y=";
          };
        });
      };

      vimPlugins = prev.vimPlugins // (import ./vim-plugins.nix { pkgs = prev; });

      yabai = prev.yabai.overrideAttrs (_: rec {
        version = "3.3.10";
        src = builtins.fetchTarball {
          url =
            "https://github.com/koekeishiya/yabai/releases/download/v${version}/yabai-v${version}.tar.gz";
            sha256 = "sha256:1z95njalhvyfs2xx6d91p9b013pc4ad846drhw0k5gipvl03pp92";
          };
        });
      })
    ];
  }
