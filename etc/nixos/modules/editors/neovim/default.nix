{ config, pkgs, inputs, lib, ... }:

let
  inherit (builtins) listToAttrs readFile toJSON;
  inherit (lib) mkEnableOption mkIf mkMerge mkOption nameValuePair types;
  inherit (lib.internal.options) mkEnableOpt';
  inherit (config.nixpkgs) system;

  mkOptional = plugin: {
    inherit plugin;
    optional = true;
  };

  treesitter = pkgs.vimPlugins.nvim-treesitter.withPlugins (plugins:
    with plugins; [
      tree-sitter-nix
      tree-sitter-fennel
      tree-sitter-lua
      tree-sitter-bash
      tree-sitter-fish
      tree-sitter-go
      tree-sitter-rust
      tree-sitter-yaml
      tree-sitter-json
      tree-sitter-markdown
    ]);

  basePlugins = with pkgs.vimPlugins; [
    impatient-nvim
    hotpot-nvim
    zest-nvim
    nvim-lua
    guihua-lua
    nvim-web-devicons
    vim-repeat
    project-nvim
  ];

  optionalPlugins = with pkgs.vimPlugins; [
    # Base
    plenary-nvim
    popup-nvim
    packer-nvim

    # UI
    feline-nvim
    barbar-nvim
    indent-blankline-nvim
    nvim-tree-lua
    neoscroll-nvim
    nvim-scrollview

    # Editor
    vim-surround
    vim-matchup
    lightspeed-nvim
    nvim-autopairs
    comment-nvim

    # Code
    vim-nix
    fennel-vim
    treesitter
    nvim-treesitter-textobjects
    nvim-treesitter-refactor
    nvim-lspconfig
    null-ls-nvim
    trouble-nvim
    lsp_signature-nvim
    navigator-lua
    nvim-cmp
    cmp-buffer
    cmp-path
    cmp-spell
    cmp-look
    cmp-calc
    cmp-treesitter
    cmp-nvim-lsp
    cmp-nvim-lua
    cmp_luasnip
    luasnip
    friendly-snippets

    # Tools
    telescope-nvim
    telescope-frecency-nvim
    telescope-fzf-native-nvim
    gitsigns-nvim
    neogit
    diffview-nvim
    git-worktree-nvim
    tmux-nvim
    toggleterm-nvim
    which-key-nvim
  ];

  cfg = config.modules.editors.neovim;
in {
  options.modules.editors.neovim = { enable = mkEnableOption "Neovim editor"; };

  config.hm = mkIf cfg.enable (mkMerge [{
    programs.neovim = {
      enable = true;
      package = pkgs.neovim-nightly;
      vimAlias = true;
      vimdiffAlias = true;
      withNodeJs = true;
      withPython3 = true;

      extraConfig = ''
        lua << EOF
        require'impatient'
        require'hotpot'
        require'zest'.setup()
        require'core'
        EOF
      '';

      plugins = basePlugins ++ (map mkOptional optionalPlugins);

      extraPackages = with pkgs; [
        gcc
        gnumake
        sqlite
        sumneko-lua-language-server
        nodePackages.bash-language-server
        nixfmt
        selene
        stylua
        fennel
        fnlfmt
        shellcheck
        shfmt
        shellharden
        vale
        codespell
      ];
    };

    home.file = {
      ".config/nvim/lua/colors.lua".text = let c = config.modules.theme.colors;
      in ''
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
  }]);
}
