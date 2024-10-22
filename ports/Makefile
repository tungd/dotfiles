COMMIT_HASH := 246d68bd2a5468bd428a0406402e7ad2b3005314

PortIndex: editors/emacs-weekly/Portfile
	portindex

emacs-weekly:
	curl -LO 'https://codeload.github.com/emacs-mirror/emacs/legacy.tar.gz/$(COMMIT_HASH)?dummy='
	shasum -a 256 $(COMMIT_HASH)
	echo 'Latest revision: '
	echo 'Latest sha: '

.PHONY: emacs-weekly PortIndex
