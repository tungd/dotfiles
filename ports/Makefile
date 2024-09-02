COMMIT_HASH := 92ea393a16e5c99a8860dab368c6ca3ca6abc3c5

PortIndex: editors/emacs-dev/Portfile
	portindex

emacs-dev:
	curl -LO 'https://codeload.github.com/emacs-mirror/emacs/legacy.tar.gz/$(COMMIT_HASH)?dummy='
	shasum -a 256 $(COMMIT_HASH)
	echo 'Latest revision: '
	echo 'Latest sha: '

.PHONY: emacs-dev PortIndex
