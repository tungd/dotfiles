{
  packageOverrides = pkgs_: with pkgs_; {
    all = with pkgs; buildEnv {
      name = "all";
      paths = [
        ag
        aspell
        aspellDicts.en
        awscli
        bashCompletion
        bundler
        closurecompiler
        elixir
        ghc
        git
        # gnupg
        (gnupg.override {
          x11Support = false;
        })
        gnutls
        imagemagick
        # lftp
        mosh
        nodejs-6_x
        postgresql
        python3
        python35Packages.youtube-dl-light
        ruby
        sqlite
        stack
        tmux
        tree
        wget
      ];
    };
  };
}
