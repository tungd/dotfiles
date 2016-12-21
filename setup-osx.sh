defaults write -g ApplePressAndHoldEnabled -bool false
defaults write NSGlobalDomain KeyRepeat -int 0
defaults write -g NSUserKeyEquivalents -dict-add 'Emoji & Symbols' '@^s'

echo 'pinentry-program /Users/tung/.nix-profile/Applications/pinentry-mac.app/Contents/MacOS/pinentry-mac' >> ~/.gnupg/gpg-agent.conf

defaults write com.apple.screencapture disable-shadow -bool true
killall SystemUIServer

# Fix macOS Sierra ssh-agent issue
echo >> ~/.ssh/config <EOF
Host *
    IdentityFile ~/.ssh/id_rsa
    AddKeysToAgent yes
EOF
