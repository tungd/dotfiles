EMACS := $(shell curl -s "https://api.github.com/repos/emacs-mirror/emacs/commits/master" | grep '"sha"' | head -1 | sed 's/.*"sha": "\([^"]*\)".*/\1/')
LLAMA := $(shell curl -s "https://api.github.com/repos/ikawrakow/ik_llama.cpp/commits/main" | grep '"sha"' | head -1 | sed 's/.*"sha": "\([^"]*\)".*/\1/')
TODAY := $(shell date "+%Y%m%d")
NIX_PROFILE_DIR := ./nix/profile
EMACS_WEEKLY_NIX := $(NIX_PROFILE_DIR)/packages/emacs-weekly.nix
NIX_PROFILE_NAME := nix-profile
NIX_PROFILE_KEEP_DAYS ?= 7

.DEFAULT_GOAL := emacs-weekly

.PHONY: emacs-weekly emacs-weekly-update emacs-weekly-build emacs-weekly-upgrade nix-profile-flake-update nix-profile-clean fix-emacs-shims

ports/PortIndex: ports/editors/emacs-weekly/Portfile ports/llm/ik_llama.cpp/Portfile
	cd ports && portindex

$(EMACS).tar.gz:
	curl -LO 'https://github.com/emacs-mirror/emacs/archive/$(EMACS).tar.gz'

$(LLAMA).tar.gz:
	curl -LO 'https://github.com/ikawrakow/ik_llama.cpp/archive/$(LLAMA).tar.gz'

ports/editors/emacs-weekly/Portfile: $(EMACS).tar.gz ports/editors/emacs-weekly/Portfile.tmpl
	sed -e 's/<COMMIT_HASH>/$(EMACS)/g' ports/editors/emacs-weekly/Portfile.tmpl \
		| sed -e "s/<SHA_256>/$$(shasum -a 256 $(EMACS).tar.gz | cut -w -f1)/g" \
		| sed -e 's/<DATE>/$(TODAY)/g' \
		> $@

ports/llm/ik_llama.cpp/Portfile: $(LLAMA).tar.gz ports/llm/ik_llama.cpp/Portfile.tmpl
	sed -e 's/<COMMIT_HASH>/$(LLAMA)/g' ports/llm/ik_llama.cpp/Portfile.tmpl \
		| sed -e "s/<SHA_256>/$$(shasum -a 256 $(LLAMA).tar.gz | cut -w -f1)/g" \
		| sed -e 's/<DATE>/$(TODAY)/g' \
		> $@

emacs-weekly: nix-profile-flake-update emacs-weekly-update emacs-weekly-build emacs-weekly-upgrade fix-emacs-shims nix-profile-clean

nix-profile-flake-update:
	nix flake update --flake "$(NIX_PROFILE_DIR)"

emacs-weekly-update:
	@set -eu; \
	rev="$(EMACS)"; \
	date="$(TODAY)"; \
	hash32="$$(nix-prefetch-url --unpack "https://github.com/emacs-mirror/emacs/archive/$$rev.tar.gz")"; \
	hash="$$(nix hash convert --hash-algo sha256 --from nix32 --to sri "$$hash32")"; \
	perl -0pi -e 's#versionDate = "[^"]+";#versionDate = "'$$date'";#; s#rev = "[^"]+";#rev = "'$$rev'";#; s#hash = "[^"]+";#hash = "'$$hash'";#;' "$(EMACS_WEEKLY_NIX)"; \
	echo "Pinned emacs-weekly to $$rev ($$date, $$hash)"

emacs-weekly-build:
	nix build --impure --no-link "$(NIX_PROFILE_DIR)#emacs-weekly" -L

emacs-weekly-upgrade:
	nix profile upgrade --impure "$(NIX_PROFILE_NAME)"

nix-profile-clean:
	nix profile wipe-history --older-than "$(NIX_PROFILE_KEEP_DAYS)d"
	nix store gc

fix-emacs-shims:
	mkdir -p "$$HOME/.local/bin"
	ln -sfn "$$HOME/.nix-profile/bin/emacs" "$$HOME/.local/bin/emacs"
	ln -sfn "$$HOME/.nix-profile/bin/emacsclient" "$$HOME/.local/bin/emacsclient"
	ln -sfn "$$HOME/.nix-profile/bin/kl" "$$HOME/.local/bin/kl"
