COMMIT_HASH := 8c5d69998e65d3ecf5f599bd828bf3330f4f118a

PortIndex: editors/emacs-weekly/Portfile
	portindex

emacs-weekly:
	curl -LO 'https://codeload.github.com/emacs-mirror/emacs/legacy.tar.gz/$(COMMIT_HASH)?dummy='
	shasum -a 256 $(COMMIT_HASH)
	echo 'Latest revision: '
	echo 'Latest sha: '

.PHONY: emacs-weekly PortIndex
