NIX_SOURCES := $(shell find -name "*.nix")
MARKUP_SOURCES := $(shell find -name "*.yaml" -or -name "*.json" -or -name "*.toml")
SH_SOURCES := $(shell find -name "*.sh")
GO_SOURCES := $(shell find -name "*.go")

PACKAGES := emacs

all: switch

.PHONY: help
help: ## Show this help screen.
	@awk 'BEGIN {FS = ":.*##"; printf "\nUsage:\n  make \033[36m<target>\033[0m\n"} /^[a-zA-Z0-9_-]+:.*?##/ { printf "  \033[36m%-25s\033[0m %s\n", $$1, $$2 } /^##@/ { printf "\n\033[1m%s\033[0m\n", substr($$0, 5) } ' $(MAKEFILE_LIST)

init:
	test -L "${HOME}/.config/git" || rm -rf "${HOME}/.config/git"
	ln -vsfn "${ETC}/git" "${HOME}/.config/git"
	test -L "${HOME}/.config/emacs" || rm -rf "${HOME}/.config/emacs"
	ln -vsfn "${ETC}/emacs" "${HOME}/.config/emacs"
	for item in bashrc xinitrc; do \
		ln -vsf {${ETC}/,${HOME}/.}$$item; \
	done

ssh:
	test -L "${HOME}/.ssh" || rm -rf "${HOME}/.config/ssh"
	ln -vsfn "${ETC}/ssh" "${HOME}/.ssh"
	sudo ln -vsf "${ETC}/authorized_keys" /etc/ssh/authorized_keys

xmonad:
	test -L "${HOME}/.config/xmonad" || rm -rf "${HOME}/.config/xmonad"
	ln -vsfn "${ETC}/xmonad" "${HOME}/.config/xmonad"
