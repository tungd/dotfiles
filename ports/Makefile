COMMIT_HASH := 4073c624376148d469a27a7c487a9b2f49d5352a

PortIndex: editors/emacs-dev/Portfile
	portindex

emacs-dev:
	curl -LO 'https://codeload.github.com/emacs-mirror/emacs/legacy.tar.gz/$(COMMIT_HASH)?dummy='
	shasum -a 256 $(COMMIT_HASH)
	echo 'Latest revision: '
	echo 'Latest sha: '

.PHONY: emacs-dev PortIndex
