COMMIT_HASH := eab14d68b2e72b9a6b8b0cc67c9667c2bfbed4f5
TODAY := $(shell date "+%Y%m%d")

ports/PortIndex: ports/editors/emacs-weekly/Portfile
	cd ports && portindex

$(COMMIT_HASH).tar.gz:
	curl -LO 'https://github.com/emacs-mirror/emacs/archive/$(COMMIT_HASH).tar.gz'

ports/editors/emacs-weekly/Portfile: $(COMMIT_HASH).tar.gz ports/editors/emacs-weekly/Portfile.tmpl
	sed -e 's/<COMMIT_HASH>/$(COMMIT_HASH)/g' ports/editors/emacs-weekly/Portfile.tmpl \
		| sed -e "s/<SHA_256>/$$(shasum -a 256 $(COMMIT_HASH).tar.gz | cut -w -f1)/g" \
		| sed -e 's/<DATE>/$(TODAY)/g' \
		> $@
