COMMIT_HASH := 1704fa4fb4164a15c7e258b922dbba190811d92d

PortIndex: editors/emacs-weekly/Portfile
	portindex

emacs-weekly:
	curl -LO 'https://codeload.github.com/emacs-mirror/emacs/legacy.tar.gz/$(COMMIT_HASH)?dummy='
	shasum -a 256 $(COMMIT_HASH)
	echo 'Latest revision: '
	echo 'Latest sha: '

.PHONY: emacs-weekly PortIndex
