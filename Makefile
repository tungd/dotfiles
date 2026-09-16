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
	yabai \
	skhd \
	clickhouse

.DEFAULT_GOAL := macports

.PHONY: macports macports-tools macports-select emacs-weekly

ports/PortIndex: ports/devel/shader-slang/Portfile
	cd ports && portindex

$(MACPORTS_LOCAL_PORTS)/PortIndex: ports/PortIndex
	rsync -a --delete --exclude .DS_Store --exclude work ports/ "$(MACPORTS_LOCAL_PORTS)/"
	cd "$(MACPORTS_LOCAL_PORTS)" && portindex

macports: macports-tools macports-select emacs-weekly

macports-tools:
	sudo port install $(MACPORTS_PACKAGES)

macports-select:
	sudo port select --set python python314
	sudo port select --set python3 python314

emacs-weekly:
	./emacs/build.sh
