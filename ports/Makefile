COMMIT_HASH := 9cfd13ff44e8d6f56a1025207c833ab45a7d51ba

PortIndex: editors/emacs-weekly/Portfile
	portindex

emacs-weekly:
	curl -LO 'https://codeload.github.com/emacs-mirror/emacs/legacy.tar.gz/$(COMMIT_HASH)?dummy='
	shasum -a 256 $(COMMIT_HASH)
	echo 'Latest revision: '
	echo 'Latest sha: '

.PHONY: emacs-weekly PortIndex
