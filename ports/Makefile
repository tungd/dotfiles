COMMIT_HASH := 944e45db53cb173c5eadd4794081c133e8649d67

PortIndex: editors/emacs-dev/Portfile
	portindex

emacs-dev:
	curl -LO 'https://codeload.github.com/emacs-mirror/emacs/legacy.tar.gz/$(COMMIT_HASH)?dummy='
	shasum -a 256 $(COMMIT_HASH)
	echo 'Latest revision: '
	echo 'Latest sha: '

.PHONY: emacs-dev PortIndex
