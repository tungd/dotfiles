COMMIT_HASH := 7811a7d38bb7cb303dc66efa02eb95e75a03f39d

PortIndex: editors/emacs-dev/Portfile
	portindex

emacs-dev:
	curl -LO 'https://codeload.github.com/emacs-mirror/emacs/legacy.tar.gz/$(COMMIT_HASH)?dummy='
	shasum -a 256 $(COMMIT_HASH)
	echo 'Latest revision: '
	echo 'Latest sha: '

.PHONY: emacs-dev PortIndex
