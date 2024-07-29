COMMIT_HASH := e4d22abcab60ead179e7d114faa4c2def559cfbb

PortIndex: editors/emacs-dev/Portfile
	portindex

emacs-dev:
	curl -LO 'https://codeload.github.com/emacs-mirror/emacs/legacy.tar.gz/$(COMMIT_HASH)?dummy='
	shasum -a 256 $(COMMIT_HASH)
	echo 'Latest revision: '
	echo 'Latest sha: '

.PHONY: emacs-dev PortIndex
