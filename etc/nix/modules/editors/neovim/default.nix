{ config, pkgs, ... }:

let
  mkOptional = plugin: {
    inherit plugin;
    optional = true;
  };

  treesitter = pkgs.vimPlugins.nvim-treesitter.withPlugins (plugins:
    with plugins; [
      tree-sitter-nix
      # tree-sitter-fennel
      tree-sitter-lua
      tree-sitter-bash
      tree-sitter-fish
      tree-sitter-go
      tree-sitter-rust
      tree-sitter-yaml
      tree-sitter-json
      tree-sitter-markdown
    ]);
in {
  hm = {
    programs.neovim = {
      enable = true;
      package = pkgs.neovim-nightly;
      vimAlias = true;
      vimdiffAlias = true;
      withNodeJs = true;
      withPython3 = true;

      extraConfig = ''
        let &runtimepath=split(&runtimepath, ",")[0] . expand(",$HOME/Workshop/etc/nvim,$HOME/.config/nvim,$HOME/.local/share/nvim/site,$VIMRUNTIME")
        let &packpath=&runtimepath

        lua << EOF
        require'impatient
        require'hotpot'
        require'zest'.setup()
        require'core'
        EOF
      '';

      plugins = with pkgs.vimPlugins; [
        impatient-nvim
        nvim-lua
        hotpot-nvim
        zest-nvim
        (mkOptional packer-nvim)
        (mkOptional treesitter)
        (mkOptional sqlite-lua)
      ];

      extraPackages = with pkgs; [
        gcc
        gnumake
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
      ".config/nvim/lua/colors.lua".text = with config.theme.colors; ''
        return {
          bg0 = '${bg0}',
          bg1 = '${bg1}',
          bg2 = '${bg2}',
          bg3 = '${bg3}',
          fg0 = '${fg0}',
          fg1 = '${fg1}',
          fg2 = '${fg2}',
          fg3 = '${fg3}',
          alert = '${alert}',
          primary = '${primary}',
          secondary = '${secondary}',
          tertiary = '${tertiary}',
          quaternary = '${quaternary}',
          quinary = '${quinary}',
          senary = '${senary}',
          septary = '${septary}',
        }
      '';
    };
  };
}
