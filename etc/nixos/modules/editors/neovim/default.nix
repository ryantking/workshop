{ config, pkgs, inputs, lib, ... }:

let
  inherit (builtins) listToAttrs readFile toJSON;
  inherit (lib) mkEnableOption mkIf mkMerge mkOption nameValuePair types;
  inherit (lib.internal.options) mkEnableOpt';
  inherit (config.nixpkgs) system;

  cfg = config.modules.editors.neovim;
in {
  options.modules.editors.neovim = {
    enable = mkEnableOption "Neovim editor";
  };

  config = mkIf cfg.enable (mkMerge [
    {
      hm.programs.neovim = {
        enable = true;
        package = pkgs.neovim-nightly;
        vimAlias = true;
        vimdiffAlias = true;
        withNodeJs = true;
        withPython3 = true;

	extraConfig = ''
lua<<EOF
vim.g["aniseed#env"] = { module = "core", compile = false }
EOF
	'';

        plugins = with pkgs.vimPlugins; [
	  { plugin = aniseed; }
	  { plugin = packer-nvim; }
	  {
	    plugin = pkgs.fetchFromGitHub {
	      owner = "tsbohc";
	      repo = "zest.nvim";
	      rev = "06cd8b2c35f0cda66f348fa3a6225c746cad2edc";
	      sha256 = "sha256-WQJWMYyhHAIuy8YQPZgbnS1oG0nOyqPqemvs0YBnFP8=";
	    };
	  }

          # temporary until packer is going
          vim-addon-nix 
          vim-fish
        ];
      };

      hm.home.file = {
        ".config/nvim/fnl".source = ./config/fnl;
        ".config/nvim/after".source = ./config/after;
        ".config/nvim/spell".source = ./config/spell;
        ".config/nvim/plugin" = {
          source = ./config/plugin;
          recursive = true;
        };
      };

      hm.home.packages = with pkgs; [ ctags neovim-remote fzf sqlite fennel gcc ];
    }
  ]);
}
