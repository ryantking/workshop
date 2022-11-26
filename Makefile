PACKAGES := emacs

all: help

.PHONY: help
help: ## Show this help screen.
	@awk 'BEGIN {FS = ":.*##"; printf "\nUsage:\n  make \033[36m<target>\033[0m\n"} /^[a-zA-Z0-9_-]+:.*?##/ { printf "  \033[36m%-25s\033[0m %s\n", $$1, $$2 } /^##@/ { printf "\n\033[1m%s\033[0m\n", substr($$0, 5) } ' $(MAKEFILE_LIST)

UNAME := $(shell uname -s | tr A-Z a-z)
SYSTEM_ROOT := "${HOME}/System"
ETC := "${SYSTEM_ROOT}/etc"

##@ Config Files

.PHONY: init xmonad

init:
	test -L "${HOME}/.config/git" || rm -rf "${HOME}/.config/git"
	ln -vsfn "${ETC}/git" "${HOME}/.config/git"
	for item in bashrc; do \
		ln -vsf {${ETC}/,${HOME}/.}$$item; \
	done

emacs:
ifeq ($(UNAME),darwin)
	brew tap d12frosted/emacs-plus
	brew install --with-elrumo1-icon emacs-plus@29
	brew services start emacs-plus@29
endif
	test -L "${HOME}/.config/emacs" || rm -rf "${HOME}/.config/emacs"
	ln -vsfn "${ETC}/emacs" "${HOME}/.config/emacs"

gnupg:
ifeq ($(UNAME),darwin)
	brew install --cask gpg-suite-no-mail
	brew install pinentry-mac
endif
	test -L "${HOME}/.gnupg" || rm -rf "${HOME}/.gnupg"
	ln -vsfn "${ETC}/gnupg" "${HOME}/.gnupg"
	export GPG_TTY="$(tty)"
	export SSH_AUTH_SOCK=$(gpgconf --list-dirs agent-ssh-socket)
	gpgconf --launch gpg-agent

##@ Linux

xmonad:
	test -L "${HOME}/.config/xmonad" || rm -rf "${HOME}/.config/xmonad"
	ln -vsfn "${ETC}/xmonad" "${HOME}/.config/xmonad"

linux:
	for item in xinitrc; do \
		ln -vsf ${ETC}/$$item ${HOME}/.$$item

##@ Darwin

.PHONY: darwin yabai hammerspoon sketchybar

darwin: yabai sketchybar

yabai:
	brew tap koekeishiya/formulae
	brew install yabai skhd
	ln -vsf ${ETC}/yabairc "${HOME}/.yabairc"
	ln -vsf ${ETC}/skhdrc "${HOME}/.skhdrc"
	brew services start yabai
	brew services start skhd

sketchybar:
	brew tap FelixKratz/formulae
	brew install sketchybar ifstat
	test -L "${HOME}/.config/sketchybar" || rm -rf "${HOME}/.config/sketchybar"
	ln -vsfn "${ETC}/sketchybar" "${HOME}/.config/sketchybar"
	brew services start sketchybar
