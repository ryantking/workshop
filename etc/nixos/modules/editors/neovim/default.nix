{ config, pkgs, inputs, lib, ... }:

let
  inherit (builtins) listToAttrs readFile toJSON;
  inherit (lib) mkEnableOption mkIf mkMerge mkOption nameValuePair types;
  inherit (lib.internal.options) mkEnableOpt';
  inherit (config.nixpkgs) system;

  pluginGit = ref: repo: pkgs.vimUtils.buildVimPluginFrom2Nix {
    pname = "${lib.strings.sanitizeDerivationName repo}";
    version = ref;
    src = builtins.fetchGit {
      url = "https://github.com/${repo}.git";
      ref = ref;
    };
  };

  plugin = pluginGit "HEAD";

  guihua-lua = (plugin "ray-x/guihua.lua") // {
    buildPhase = "(cd lua/fzy && make)";
  };

  withLua = plugin: lua: {
    inherit plugin;
    config = "lua ${lua}";
  };

  cfg = config.modules.editors.neovim;
in
{
  options.modules.editors.neovim = { enable = mkEnableOption "Neovim editor"; };

  config.hm = mkIf cfg.enable (mkMerge [
    {
      programs.neovim = {
        enable = true;
        package = pkgs.neovim-nightly;
        vimAlias = true;
        vimdiffAlias = true;
        withNodeJs = true;
        withPython3 = true;

        extraConfig = "lua require 'core'";

        plugins = with pkgs.vimPlugins; [
          (withLua (plugin "rktjmp/hotpot.nvim") "require'hotpot'")
          (withLua (plugin "tsbohc/zest.nvim") "require'zest'.setup()")
          (plugin "norcalli/nvim.lua") 
          guihua-lua

          plenary-nvim
          popup-nvim
          nvim-web-devicons

          (withLua nvim-treesitter "require'plugins.treesitter'")
          vim-surround
          vim-repeat
          lightspeed-nvim
          (withLua (plugin "numToStr/Comment.nvim" ) "require'Comment'.setup()") 
          (withLua nvim-autopairs "require'plugins.autopairs'")

          (withLua feline-nvim "require'plugins.feline'")
          barbar-nvim
          (withLua neoscroll-nvim "require'neoscroll'.setup()")
          (withLua indent-blankline-nvim "require'plugins.indent-blankline'")
          nvim-scrollview

          (withLua nvim-tree-lua "require'nvim-tree'.setup {}")
          (withLua telescope-nvim "require'plugins.telescope'")
          telescope-frecency-nvim
          telescope-fzf-native-nvim
          (plugin "ahmedkhalf/project.nvim")

          (withLua gitsigns-nvim "require'gitsigns'.setup()")
          diffview-nvim
          (withLua git-worktree-nvim "require'git-worktree'.setup()")
          (withLua neogit "require'plugins.git'")

          vim-nix
          fennel-vim

          (withLua nvim-lspconfig "require'plugins.lsp'")
          null-ls-nvim
          trouble-nvim
          lsp_signature-nvim
          (plugin "ray-x/navigator.lua")

          (withLua which-key-nvim "require'plugins.which-key'")
          (withLua (plugin "aserowy/tmux.nvim") "require'plugins.tmux'")
          (withLua toggleterm-nvim "require'plugins.term'")
        ];

        extraPackages = with pkgs; [
          gcc
          gnumake
          sqlite
          sumneko-lua-language-server
          nodePackages.bash-language-server
          selene
          stylua
          fennel
          fnlfmt
          shellcheck
          shfmt
          shellharden
        ];
      };

      home.file = {
        ".config/nvim/lua/colors.lua".text = let c = config.modules.theme.colors; in ''
          return {
            bg0 = '${c.bg0}',
            bg1 = '${c.bg1}',
            bg2 = '${c.bg2}',
            bg3 = '${c.bg3}',
            fg0 = '${c.fg0}',
            fg1 = '${c.fg1}',
            fg2 = '${c.fg2}',
            fg3 = '${c.fg3}',
            alert = '${c.alert}',
            primary = '${c.primary}',
            secondary = '${c.secondary}',
            tertiary = '${c.tertiary}',
            quaternary = '${c.quaternary}',
            quinary = '${c.quinary}',
            senary = '${c.senary}',
            septary = '${c.septary}',
          }
        '';
      };
    }
  ]);
}
