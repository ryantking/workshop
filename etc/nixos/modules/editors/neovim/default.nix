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

  cfg = config.modules.editors.neovim;
in
{
  options.modules.editors.neovim = { enable = mkEnableOption "Neovim editor"; };

  config = mkIf cfg.enable (mkMerge [
    {
      hm.programs.neovim = {
        enable = true;
        package = pkgs.neovim-nightly;
        vimAlias = true;
        vimdiffAlias = true;
        withNodeJs = true;
        withPython3 = true;

/*
        extraConfig = "lua require'core'";
        /*   "let g:sqlite_clib_path = '${pkgs.sqlite.out}/lib/libsqlite3.so'\n" + */
        /*   ''let g:conjure#client#fennel#aniseed#aniseed_module_prefix = "aniseed."''; */

        plugins = with pkgs.vimPlugins; [
          {
            plugin = (plugin "rktjmp/hotpot.nvim");
            config = "lua require'hotpot'";
          }
          {
            plugin = (plugin "tsbohc/zest.nvim");
            config = "lua require'zest'.setup()";
          }
        ];

        extraPackages = with pkgs; [
          gnumake
          sqlite
          selene
          stylua
          fennel
          fnlfmt
          shellcheck
          shfmt
          shellharden
        ];
	*/
      };

      /* hm.home.file = { */
      /*   ".config/nvim/fnl".source = ./config/fnl; */
      /*   ".config/nvim/after".source = ./config/after; */
      /*   ".config/nvim/spell".source = ./config/spell; */
      /*   ".config/nvim/plugin" = { */
      /*     source = ./config/plugin; */
      /*     recursive = true; */
      /*   }; */
      /* }; */

      # hm.home.packages = with pkgs; [ ctags neovim-remote ];
    }
    /*
    (mkIf config.modules.devel.nix.enable {
      hm.programs.neovim = {
        # extraPackages = with pkgs; [ inputs.rnix-lsp.defaultPackage.${system} ];
        # extraConfig = "lua require'lspconfig'.rnix.setup{}";
      };
    })
    */
  ]);
}
