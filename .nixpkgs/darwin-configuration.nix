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
  system.defaults.NSGlobalDomain.InitialKeyRepeat = 10;
  system.defaults.NSGlobalDomain.KeyRepeat = 1;

  system.defaults.dock.autohide = true;
  system.defaults.dock.orientation = "left";

  system.keyboard.enableKeyMapping = true;
  system.keyboard.remapCapsLockToControl = true;


  environment.systemPackages =
    [ pkgs.ag
      pkgs.antiword
      pkgs.aria2
      pkgs.aspell
      pkgs.aspellDicts.en
      pkgs.bashCompletion
      pkgs.closurecompiler
      pkgs.curl
      pkgs.gettext
      pkgs.gnupg
      pkgs.imagemagick
      pkgs.jdk
      pkgs.mosh
      pkgs.mysql.client
      pkgs.nixops
      pkgs.nodejs-8_x
      pkgs.openssl
      pkgs.optipng
      pkgs.pinentry_mac
      pkgs.pkgconfig
      pkgs.poppler_utils
      pkgs.postgresql
      pkgs.sqlite
      pkgs.stack
      pkgs.tmux
      pkgs.tree
      pkgs.watchman
      pkgs.wget

      pkgs.nix-repl
    ];

  programs.bash.enable = true;
  programs.bash.enableCompletion = false;

  services.activate-system.enable = true;
  services.nix-daemon.enable = true;

  nix.maxJobs = 2;
  nix.buildCores = 2;
  nix.gc.automatic = true;

  networking.hostName = "tung-mbp";
}
