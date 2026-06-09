EMACS := $(shell curl -s "https://api.github.com/repos/emacs-mirror/emacs/commits/master" | grep '"sha"' | head -1 | sed 's/.*"sha": "\([^"]*\)".*/\1/')
LLAMA := $(shell curl -s "https://api.github.com/repos/ikawrakow/ik_llama.cpp/commits/main" | grep '"sha"' | head -1 | sed 's/.*"sha": "\([^"]*\)".*/\1/')
TODAY := $(shell date "+%Y%m%d")
MACPORTS_LOCAL_PORTS := /opt/local/var/macports/sources/local/dotfiles-ports

MACPORTS_PACKAGES := \
	cloudflared \
	coreutils \
	curl-ca-bundle \
	direnv \
	duckdb \
	fd \
	fzf \
	gh \
	git \
	git-lfs \
	gnupg2 \
	htop \
	jq \
	mysql84 \
	nodejs24 \
	npm11 \
	opam \
	pinentry-mac \
	plantuml \
	postgresql16 \
	py314-certifi \
	python314 \
	python_select \
	python3_select \
	ripgrep \
	sqlite3 \
	tmux \
	tokei \
	tree \
	universal-ctags \
	uv \
	valkey \
	wget \
	clickhouse

.DEFAULT_GOAL := macports

.PHONY: macports macports-tools macports-select emacs-weekly emacs-weekly-update fix-emacs-shims

ports/PortIndex: ports/editors/emacs-weekly/Portfile ports/llm/ik_llama.cpp/Portfile
	cd ports && portindex

$(MACPORTS_LOCAL_PORTS)/PortIndex: ports/PortIndex
	rsync -a --delete --exclude .DS_Store --exclude work ports/ "$(MACPORTS_LOCAL_PORTS)/"
	cd "$(MACPORTS_LOCAL_PORTS)" && portindex

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

macports: macports-tools macports-select emacs-weekly fix-emacs-shims

macports-tools:
	sudo port install $(MACPORTS_PACKAGES)

macports-select:
	sudo port select --set python python314
	sudo port select --set python3 python314

emacs-weekly: emacs-weekly-update $(MACPORTS_LOCAL_PORTS)/PortIndex
	sudo port install emacs-app-devel +nativecomp +treesitter

emacs-weekly-update: ports/editors/emacs-weekly/Portfile

fix-emacs-shims:
	mkdir -p "$$HOME/.local/bin"
	rm -f "$$HOME/.local/bin/emacs" "$$HOME/.local/bin/emacsclient"
