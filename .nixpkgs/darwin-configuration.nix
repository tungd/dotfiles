{ config, pkgs, ... }:
{
  system.stateVersion = 2;

  system.defaults.finder.AppleShowAllExtensions = true;
  system.defaults.finder.QuitMenuItem = true;

  system.defaults.trackpad.TrackpadThreeFingerDrag = true;

  system.defaults.NSGlobalDomain.ApplePressAndHoldEnabled = false;
  system.defaults.NSGlobalDomain.AppleShowAllExtensions = true;
  system.defaults.NSGlobalDomain.NSAutomaticCapitalizationEnabled = false;
  system.defaults.NSGlobalDomain.NSAutomaticSpellingCorrectionEnabled = false;
  system.defaults.NSGlobalDomain.InitialKeyRepeat = 12;
  system.defaults.NSGlobalDomain.KeyRepeat = 1;

  system.defaults.dock.autohide = true;
  system.defaults.dock.orientation = "left";

  system.keyboard.enableKeyMapping = true;
  system.keyboard.remapCapsLockToControl = true;


  environment.systemPackages = with pkgs; [
    ag
    antiword
    aria2
    # aspell
    # aspellDicts.en
    bashCompletion
    closurecompiler
    curl
    gettext
    git-lfs
    gnupg
    hlint
    imagemagick
    jdk
    mosh
    mysql.client
    nixops
    nmap
    nodejs-8_x
    openssl
    optipng
    pinentry_mac
    pkgconfig
    poppler_utils
    postgresql
    sqlite
    stack
    tmux
    tree
    # watchman
    wget

    # (import ./emacs-mac.nix)

    nix-repl
  ];

  programs.bash.enableCompletion = false;

  services.activate-system.enable = true;
  services.nix-daemon.enable = true;

  networking.hostName = "tung-mbp";

  nix.gc.automatic = true;
  nix.maxJobs = 2;
  nix.buildCores = 2;
}
