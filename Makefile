# ############
# Workshop

all: switch

.PHONY: help
help: ## Show this help screen.
	@awk 'BEGIN {FS = ":.*##"; printf "\nUsage:\n  make \033[36m<target>\033[0m\n"} /^[a-zA-Z0-9_-]+:.*?##/ { printf "  \033[36m%-25s\033[0m %s\n", $$1, $$2 } /^##@/ { printf "\n\033[1m%s\033[0m\n", substr($$0, 5) } ' $(MAKEFILE_LIST)

##@ System

UNAME := $(shell uname -s | tr A-Z a-z)

.PHONY: switch switch-darwin switch-linux
switch: update-pkgs switch-$(UNAME) ## Switch to the new configuration
switch-darwin:
	darwin-rebuild switch --flake .
switch-linux: /etc/nixos/flake.nix
	nixos-rebuild switch --flake
/etc/nixos/flake.nix:
	ln -s flake.nix $@

##@ Flake Management

.PHONY: update
update: ## Update flake dependencies
	nix flake update

##@ Packages

.PHONY: update-pkgs
update-pkgs: etc/nix/pkgs/_sources/generated.nix
pkgs/_sources/generated.nix: etc/nix/pkgs/sources.toml
	nvfetcher -o $(dir $@) -c $<

##@ Website

.PHONY: serve-website
serve-website: garden
	cd srv/website
	hugo server -D

.PHONY: garden
garden:
	go run ./src/cmd/trowel/main.go $(HOME)/Dropbox/org/roam -x
