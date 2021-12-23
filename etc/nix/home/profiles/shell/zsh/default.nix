{ config, pkgs, ... }:

# TODO: Switch over to zplug for lazy loading shit
# TODO: Refactor omz and plugins to new files.
# TODO: Fix history substring search
# TODO: Fix FZF styling
# TODO: Move prompt to root prompt (currently starship)
# TODO: Get tmux operational.
# TODO: Checkout presto, cross check plugins I use.
# TODO: Use zplug for omz/presto?

{
  home.packages = with pkgs; [ zoxide ];

  programs = {
    fzf = let
      fdCommand = "fd --follow --hidden --exclude .git";
      fzfOpts = with config.colorscheme.colors;
      # "--no-bold --prompt='/ ' --pointer=• --marker=• --color " +
        "--no-bold --prompt='/ ' --color "
        + "fg:#${base06},bg:-1,hl:#${base0C},fg+:#${base04},bg+:-1,hl+:#${base0B},"
        + "pointer:#${base0D},info:#${base03},spinner:#${base03},header:#${base03},prompt:#${base03},marker:#${base0D}";
    in {
      enable = true;
      enableZshIntegration = true;

      defaultCommand = "${fdCommand}";
      defaultOptions = [ "${fzfOpts}" ];
      fileWidgetCommand = "${fdCommand}";
      fileWidgetOptions = [ "${fzfOpts} --preview 'bat --color=always --plain {}'" ];
      changeDirWidgetCommand = "${fdCommand} --type d";
      changeDirWidgetOptions =
        [ "${fzfOpts} --preview 'exa -l --tree --level=2 --color=always {}'" ];
    };

    zsh = {
      enable = true;
      dotDir = ".config/zsh";
      autocd = true;
      enableCompletion = true;
      enableAutosuggestions = true;
      enableVteIntegration = true;

      history = {
        path = config.programs.zsh.dotDir + "/.zsh_history";
        share = true;
        save = 10000000;
        ignoreDups = true;
        extended = true;
        size = 10000000;
      };
      
      localVariables = {
      FZF_BASE = "${pkgs.fzf}/share/fzf";
PURE_PROMPT_SYMBOL = "➜";
PURE_PROMPT_VICMD_SYMBOL = "";
      };

      oh-my-zsh = {
        enable = true;
        plugins = [
          "aliases"
          "alias-finder"
          "cargo"
          "colored-man-pages"
          "copydir"
          "copyfile"
          "cp"
          "direnv"
          "dirhistory"
          "docker"
          "extract"
          "fancy-ctrl-z"
          "fd"
          "fzf"
          "gh"
          "git"
          "golang"
          "gpg-agent"
          "grc"
          "history-substring-search"
          "ripgrep"
          "rustup"
          "safe-paste"
          "sudo"
          "systemd"
          "zoxide"
        ];
      };

      plugins = with pkgs; [
        {
          name = "zsh-async";
          src = fetchFromGitHub {
            owner = "mafredri";
            repo = "zsh-async";
            rev = "3ba6e2d1ea874bfb6badb8522ab86c1ae272923d";
            sha256 = "sha256-3hhZXL8/Ml7UlkkHBPpS5NfUGB5BqgO95UvtpptXf8E=";
          };
	  file = "async.plugin.zsh";
        }
        {
          name = "pure";
          src = "${pure-prompt}/share/zsh/site-functions";
	  file = "prompt_pure_setup";
        }
        # {
          # name = "zsh-autosuggestions";
          # src = "${zsh-autosuggestions}/share/zsh-autosuggestions";
          # file = "zsh-autosuggestions.zsh";
        # }
        {
          name = "zsh-history-substring-search";
          src = "${zsh-history-substring-search}/share/zsh-history-substring-search";
          file = "zsh-history-substring-search.zsh";
        }
        {
          name = "zsh-fast-syntax-highlighting";
          src = "${zsh-fast-syntax-highlighting}/share/zsh/site-functions";
          file = "fast-syntax-highlighting.plugin.zsh";
        }
        {
          name = "zsh-vi-mode";
          src = zsh-vi-mode;
          file = "share/zsh-vi-mode/zsh-vi-mode.plugin.zsh";
        }
        {
          name = "zsh-256color";
          src = fetchFromGitHub {
            owner = "chrissicool";
            repo = "zsh-256color";
            rev = "9d8fa1015dfa895f2258c2efc668bc7012f06da6";
            sha256 = "14pfg49mzl32ia9i9msw9412301kbdjqrm7gzcryk4wh6j66kps1";
          };
        }
        {
          name = "zsh-fzf-tab";
          src = "${zsh-fzf-tab}/share/fzf-tab";
          file = "fzf-tab.plugin.zsh";
        }
        {
          name = "zsh-bd";
          src = "${zsh-bd}/share/zsh-bd";
          file = "bd.zsh";
        }
        {
          name = "zsh-nix-shell";
          src = "${zsh-nix-shell}/share/zsh-nix-shell";
          file = "nix-shell.plugin.zsh";
        }
      ];

      initExtraFirst = with pkgs; ''

setopt HIST_IGNORE_ALL_DUPS
setopt no_extendedglob

# Fix plugins that need bindings as vi-mode nukes them.
function zvm_after_init() {
source ${oh-my-zsh}/share/oh-my-zsh/plugins/fzf/fzf.plugin.zsh
source ${oh-my-zsh}/share/oh-my-zsh/plugins/fancy-ctrl-z/fancy-ctrl-z.plugin.zsh
source ${zsh-fzf-tab}/share/fzf-tab/fzf-tab.plugin.zsh
source ${zsh-history-substring-search}/share/zsh-history-substring-search/zsh-history-substring-search.zsh
}
      '';
    };
  };
}
