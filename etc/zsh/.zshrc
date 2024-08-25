# -*- mode: sh -*-
# shellcheck disable=all

# .zshrc: Configuration for the zoomer shell

SHELL_CONFIG_HOME="$HOME/System/etc/shell"
ZPLUGIN_DIR="$HOME/.local/share/zsh/plugins"

# Load auxillary files
[ -f "$SHELL_CONFIG_HOME/profile" ] && source "$SHELL_CONFIG_HOME/profile"
[ -f "$SHELL_CONFIG_HOME/aliasrc" ] && source "$SHELL_CONFIG_HOME/aliasrc"

fpath+=($HOME/.local/src/pure)

stty stop undef		# Disable ctrl-s to freeze terminal.

# ZSH options
setopt autocd		# Automatically cd into typed directory.
setopt interactive_comments

# History configuration
HISTSIZE=10000000
SAVEHIST=10000000
HISTFILE="${XDG_CACHE_HOME:-$HOME/.cache}/zsh/history"
mkdir -p $(dirname $HISTFILE)

# Configure colors and the prompt
source "$ZPLUGIN_DIR/F-Sy-H/F-Sy-H.plugin.zsh"
PURE_PROMPT_SYMBOL="%#"
autoload -U colors; colors
autoload -U promptinit; promptinit; prompt pure
# source "$HOME/System/etc/zsh/prompt.zsh"
# precmd_functions+=(prompt_command)
# 
# precmd() {
#     prompt_command
# }

# Basic auto/tab complete:
fpath+=~/.zfunc
autoload -U compinit; compinit
zstyle ':completion:*' menu select
zmodload zsh/complist
_comp_options+=(globdots)		# Include hidden files.

# vi mode
bindkey -v
bindkey -v '^?' backward-delete-char
export KEYTIMEOUT=1

# Change cursor shape for different vi modes.
function zle-keymap-select () {
    case $KEYMAP in
        vicmd) echo -ne '\e[1 q';;      # block
        viins|main) echo -ne '\e[5 q';; # beam
    esac
}

zle-line-init() {
    zle -K viins # initiate `vi insert` as keymap (can be removed if `bindkey -V` has been set elsewhere)
    echo -ne "\e[5 q"
}

zle -N zle-keymap-select
zle -N zle-line-init

echo -ne '\e[5 q' # Use beam shape cursor on startup.
preexec() { echo -ne '\e[5 q' ;} # Use beam shape cursor for each new prompt.

# GPG agent
export GPG_TTY="$(tty)"
export SSH_AUTH_SOCK=$(gpgconf --list-dirs agent-ssh-socket)
gpgconf --launch gpg-agent

# fzy
source "$ZPLUGIN_DIR/zsh-fzy/zsh-fzy.plugin.zsh"

zstyle :fzy:file command rg --files
zstyle :fzy:cd command find -L $HOME $HOME/Projects $HOME/.local/src -maxdepth 1 -type d

bindkey "\ec" fzy-cd-widget
bindkey "^F" fzy-file-widget
bindkey "^R" fzy-history-widget
bindkey "^P" fzy-proc-widget

# dtach session manager
# source "$HOME/System/etc/zsh/dtach.zsh"
# bindkey "^A" dtach-session-widget

# 1Password CLI completion
eval "$(op completion zsh)"
compdef _op op

# emacs integration
source "$ZPLUGIN_DIR/zsh-emacs/emacs.plugin.zsh"

# kubernetes plugin
source "$HOME/.local/src/kubectl-aliases/.kubectl_aliases"

# substring search
source "$ZPLUGIN_DIR/zsh-history-substring-search/zsh-history-substring-search.zsh"
bindkey '^[[A' history-substring-search-up
bindkey '^[[B' history-substring-search-down

# local configuration
[ -f "$HOME/.zshrc.local" ] && source "$HOME/.zshrc.local" 

# .zshrc ends here
