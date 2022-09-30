NIX_SOURCES := $(shell find -name "*.nix")
MARKUP_SOURCES := $(shell find -name "*.yaml" -or -name "*.json" -or -name "*.toml")
SH_SOURCES := $(shell find -name "*.sh")
GO_SOURCES := $(shell find -name "*.go")

all: switch

.PHONY: help
help: ## Show this help screen.
	@awk 'BEGIN {FS = ":.*##"; printf "\nUsage:\n  make \033[36m<target>\033[0m\n"} /^[a-zA-Z0-9_-]+:.*?##/ { printf "  \033[36m%-25s\033[0m %s\n", $$1, $$2 } /^##@/ { printf "\n\033[1m%s\033[0m\n", substr($$0, 5) } ' $(MAKEFILE_LIST)

##@ System

UNAME := $(shell uname -s | tr A-Z a-z)

.PHONY: switch switch-darwin switch-linux
switch: switch-$(UNAME) ## Switch to the new configuration
switch-darwin:
	darwin-rebuild switch --flake .
switch-linux: /etc/nixos/flake.nix
	sudo nixos-rebuild switch
/etc/nixos/flake.nix:
	sudo ln -s flake.nix $@

##@ Flake Management

.PHONY: update
update: ## Update flake dependencies
	nvfetcher -o etc/nix/pkgs -c etc/nix/pkgs/sources.toml
	nix flake update

##@ Formatting

.PHONY: format format-nix format-markup format-sh format-go

format: ## Format all files in the repository.
format: format-nix format-markup format-sh format-go

format-nix: ## Format Nix files with Alejandra.
	alejandra $(NIX_SOURCES)

format-prettier: ## Format markup language files with Prettier.
	yarn run prettier --write $(MARKUP_SOURCES)

format-sh: ## Format shell fines with shfmt.
	shfmt -w -i 4 $(SH_SOURCES)

format-go: ## Format go files with gofumt.
	gofumpt -w $(GO_SOURCES)

##@ Website

.PHONY: serve-website
serve-website: garden
	cd srv/website
	hugo server -D

.PHONY: garden
garden:
	go run ./bin/trowel.go $(HOME)/Dropbox/org/roam
