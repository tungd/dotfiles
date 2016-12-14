{
  packageOverrides = pkgs_: with pkgs_; {
    closurecompilerJdk8 = closurecompiler.override {
      jre = jdk8.jre;
    };

    all = with pkgs; buildEnv {
      name = "all";
      paths = [
        ag
        aspell
        aspellDicts.en
        bashCompletion
        closurecompilerJdk8
        # elixir
        ffmpeg
        gnupg
        imagemagick
        jdk8
        # mysql57
        # mongodb
        mosh
        # nixops
        nodejs-6_x
        openssl
        optipng
        pinentry_mac
        pkgconfig
        # postgresql
        # php
        # php70Packages.xdebug
        # php70Packages.composer
        sqlite
        stack
        tmux
        tree
        wget
      ];
    };
  };
}
