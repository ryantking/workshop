# Makefile: Manage system configurations

all: help

.PHONY: help
help: ## Show this help screen.
	@awk 'BEGIN {FS = ":.*##"; printf "\nUsage:\n  make \033[36m<target>\033[0m\n"} /^[a-zA-Z0-9_-]+:.*?##/ { printf "  \033[36m%-25s\033[0m %s\n", $$1, $$2 } /^##@/ { printf "\n\033[1m%s\033[0m\n", substr($$0, 5) } ' $(MAKEFILE_LIST)

ETC := $(shell pwd)/etc

install: shell x11 apps

##@ Shell

.PHONY: shell bash git ssh gnupg tldr

shell: bash git ssh gnupg tldr
shell: ## All shell programs

bash: TARGET = "$(HOME)/.bashrc"
bash: ## Configure the bash shell
	@test -L "$(TARGET)" || rm -rf "$(TARGET)"
	@ln -vsf "$(ETC)/bashrc" "$(HOME)/.bashrc"

git: TARGET = "$(XDG_CONFIG_HOME)/git"
git: ## Configure the Git CLI
	@test -L "$(TARGET)" || rm -rf "$(TARGET)"
	@ln -vsfn "$(ETC)/git" "$(TARGET)"

ssh: TARGET = "$(HOME)/.ssh"
ssh: ## Configure the SSH client
	@test -d "$(TARGET)" || mkdir -p "$(TARGET)"
	@test -L "$(TARGET)/config" || rm -rf "$(TARGET)/config"
	@ln -vsf "$(ETC)/ssh_config" "$(TARGET)/config"

gnupg: TARGET = "$(HOME)/.gnupg"
gnupg: ## Configure the GnuPG daemon
	@test -d "$(TARGET)" || mkdir -p "$(TARGET)"
	@chmod 0700 "$(TARGET)"
	@for item in gpg.conf gpg-agent.conf sshcontrol; do \
		ln -vsf {$(ETC)/gnupg/,$(TARGET),}$$item; \
	done

tldr: TARGET = "$(XDG_CONFIG_HOME)/tealdeer"
tldr: ## Configure the tealdeer tldr page viewer
	@test -d "$(TARGET)" || mkdir -p "$(TARGET)"
	@test -L "$(TARGET)/config.toml" || rm -rf "$(TARGET)/config.toml"
	@ln -vsf "$(ETC)/tealdeer.toml" "$(TARGET)/config.toml"

##@ X11

.PHONY: x11 picom dunst spotifyd

x11: picom dunst spotifyd
x11: ## Configure X11
	@ln -vsf "$(ETC)/xinitrc" "$(HOME)/.xinitrc"
	@ln -vsf "$(ETC)/Xresources" "$(HOME)/.Xresources"

picom: TARGET = "$(XDG_CONFIG_HOME)/picom.conf"
picom: ## Configure the Picom compositor
	@test -L "$(TARGET)" || rm -rf "$(TARGET)"
	@ln -vsf "${ETC}/picom.conf" "$(TARGET)"

dunst: TARGET = "$(XDG_CONFIG_HOME)/dunst"
dunst: ## Configure the Dunst notification daemon
	@test -d "$(TARGET)" || mkdir -p "$(TARGET)"
	@test -L "$(TARGET)/dunstrc" || rm -rf "$(TARGET)/dunstrc"
	@ln -vsfn "$(ETC)/dunstrc" "$(TARGET)/dunstrc"

spotifyd: TARGET = "$(XDG_CONFIG_HOME)/spotifyd.conf"
spotifyd: ## Configure the Spotify daemon
	@test -L "$(TARGET)" || rm -rf "$(TARGET)"
	@ln -vsfn "$(ETC)/spotifyd.conf" "$(TARGET)"

##@ Applications

.PHONY: apps emacs doom k9s keyd

apps: emacs k9s keyd
apps: ## Install all applications

emacs: TARGET = "$(XDG_CONFIG_HOME)/emacs"
emacs: doom ## Use Doom to configure Emacs
	@rm -rf "$(HOME)/.emacs.d"
	@if ! test -d "$(TARGET)"; then \
		git clone https://github.com/hlissner/doom-emacs "$(TARGET)"; \
		$(TARGET)/bin/doom install; \
	fi
	@$(TARGET)/bin/doom sync

doom: TARGET = "$(XDG_CONFIG_HOME)/doom"
doom: ## Configure Doom emacs
	@test -L "$(TARGET)" || rm -rf "$(TARGET)"
	@ln -vsfn "$(ETC)/doom" "$(TARGET)"

k9s: TARGET = "$(XDG_CONFIG_HOME)/k9s"
k9s: ## Configure the K9s Kubernetes UI
	@test -L "$(TARGET)" || rm -rf "$(TARGET)"
	@ln -vsfn "$(ETC)/k9s" "$(TARGET)"

keyd: TARGET = "/etc/keyd"
keyd: ## Configure keyd and its service
	@mkdir -d "$(TARGET)" || mkdir -p "$(TARGET)"
	@test -L "$(TARGET)/default.conf" || rm -rf "$(TARGET)/default.conf"
	@ln -vsfn "$(ETC)/keyd.conf" "$(TARGET)/default.conf"
	@test -L /etc/dinit.d/keyd || rm -rf /etc/dinit.d/keyd
	@ln -vsfn "$(ETC)/dinit.d/keyd" "/etc/dinit.d/keyd"

##@ Artix

.PHONY: pacman pac-freeze pac-install mkinitcpio

artix: pacman mkinitcpio
artix: ## Configure mkinitcpio

mkinitcpio: TARGET = "/etc/mkinitcpio.conf"
mkinitcpio: ## Configure mkinitcpio
	@sudo test -L $(TARGET) || sudo rm -rf "$(TARGET)"
	@sudo ln -vsfn "$(ETC)/mkinitcpio.conf" "$(TARGET)"

pacman: TARGET = "/etc/pacman.conf"
pacman: ## Configure pacman and install packages
	@sudo test -L $(TARGET) || sudo rm -rf "$(TARGET)"
	@sudo ln -vsfn "$(ETC)/pacman.conf" "$(TARGET)"

pac-freeze: ## Freeze the list of pacman files installed
	@pacman -Qe | awk '{print $$1}' > var/pacman-packages.txt

pac-install: ## Install pacman packages
	@for pkg in $(cat var/pacman-packages.txt); do pacman -S $pkg; done

# Makefile ends herer
