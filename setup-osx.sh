defaults write -g ApplePressAndHoldEnabled -bool false
defaults write -g InitialKeyRepeat -int 10
defaults write -g KeyRepeat -int 1
defaults write -g NSUserKeyEquivalents -dict-add 'Emoji & Symbols' '@^s'

# echo 'pinentry-program /Users/tung/.nix-profile/Applications/pinentry-mac.app/Contents/MacOS/pinentry-mac' >> ~/.gnupg/gpg-agent.conf

defaults write com.apple.screencapture disable-shadow -bool true
killall SystemUIServer

# Fix macOS Sierra ssh-agent issue
# echo >> ~/.ssh/config <<EOF
# Host *
#     IdentityFile ~/.ssh/id_rsa
#     AddKeysToAgent yes
#     UseKeychain yes
# EOF

# mkdir -p ~/.local/vendor
# curl -L https://raw.githubusercontent.com/rupa/z/master/z.sh > ~/.local/vendor/z.sh

# sudo pkgin install perl-5.26.0nb2 ImageMagick postgresql96-client the_silver_searcher mosh

# mkdir -p ~/.local/bin
# curl -sSL https://get.haskellstack.org/ | sh -s - -d ~/.local/bin/

# curl -o- -L https://yarnpkg.com/install.sh | bash
