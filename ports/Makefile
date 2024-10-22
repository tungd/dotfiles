COMMIT_HASH := 6864ac2bc3bee1add508872b756693a6fbe0c2e7

PortIndex: editors/emacs-weekly/Portfile
	portindex

emacs-weekly:
	curl -LO 'https://codeload.github.com/emacs-mirror/emacs/legacy.tar.gz/$(COMMIT_HASH)?dummy='
	shasum -a 256 $(COMMIT_HASH)
	echo 'Latest revision: '
	echo 'Latest sha: '

.PHONY: emacs-weekly PortIndex
