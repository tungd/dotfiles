EMACS := $(shell curl -s "https://api.github.com/repos/emacs-mirror/emacs/commits/master" | grep '"sha"' | head -1 | sed 's/.*"sha": "\([^"]*\)".*/\1/')
LLAMA := $(shell curl -s "https://api.github.com/repos/ikawrakow/ik_llama.cpp/commits/main" | grep '"sha"' | head -1 | sed 's/.*"sha": "\([^"]*\)".*/\1/')
TODAY := $(shell date "+%Y%m%d")

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
