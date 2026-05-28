{
  description = "Tung's Global macOS CLI Profile";

  inputs.nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";

  outputs = { self, nixpkgs }:
    let
      system = "aarch64-darwin"; # Apple Silicon
      pkgs = nixpkgs.legacyPackages.${system};
      emacs-weekly = pkgs.callPackage ./packages/emacs-weekly.nix { };
      db-clients = pkgs.runCommand "db-clients" { } ''
        mkdir -p $out/bin

        ln -s ${pkgs.sqlite-interactive.bin}/bin/sqlite3 $out/bin/sqlite3
        ln -s ${pkgs.duckdb}/bin/duckdb $out/bin/duckdb

        for prog in psql pg_dump pg_dumpall pg_restore pg_isready createdb dropdb createuser dropuser; do
          ln -s ${pkgs.postgresql_16}/bin/$prog $out/bin/$prog
        done

        for prog in mysql mysqladmin mysqlcheck mysql_config_editor mysqldump mysqlimport mysqlshow; do
          ln -s ${pkgs.mysql84}/bin/$prog $out/bin/$prog
        done

        ln -s ${pkgs.clickhouse}/bin/clickhouse-client $out/bin/clickhouse-client
        ln -s ${pkgs.clickhouse}/bin/clickhouse-local $out/bin/clickhouse-local
      '';
      tree-sitter-grammars =
        let
          grammars = pkgs.tree-sitter-grammars;
          grammarLinks = {
            bash = grammars.tree-sitter-bash;
            c = grammars.tree-sitter-c;
            "c-sharp" = grammars.tree-sitter-c-sharp;
            cmake = grammars.tree-sitter-cmake;
            cpp = grammars.tree-sitter-cpp;
            css = grammars.tree-sitter-css;
            dockerfile = grammars.tree-sitter-dockerfile;
            elixir = grammars.tree-sitter-elixir;
            go = grammars.tree-sitter-go;
            gomod = grammars.tree-sitter-gomod;
            heex = grammars.tree-sitter-heex;
            html = grammars.tree-sitter-html;
            java = grammars.tree-sitter-java;
            javascript = grammars.tree-sitter-javascript;
            json = grammars.tree-sitter-json;
            lua = grammars.tree-sitter-lua;
            php = grammars.tree-sitter-php;
            python = grammars.tree-sitter-python;
            ruby = grammars.tree-sitter-ruby;
            rust = grammars.tree-sitter-rust;
            scala = grammars.tree-sitter-scala;
            toml = grammars.tree-sitter-toml;
            tsx = grammars.tree-sitter-tsx;
            typescript = grammars.tree-sitter-typescript;
            yaml = grammars.tree-sitter-yaml;
          };
        in
        pkgs.runCommand "emacs-tree-sitter-grammars" { } (
          ''
            mkdir -p $out/lib/tree-sitter
          ''
          + builtins.concatStringsSep "\n" (
            pkgs.lib.mapAttrsToList (
              language: grammar:
              "ln -s ${grammar}/parser $out/lib/tree-sitter/libtree-sitter-${language}.dylib"
            ) grammarLinks
          )
        );
    in {
      packages.${system} = {
        inherit emacs-weekly;

        # This defines a custom package bundle of your favorite tools
        default = pkgs.buildEnv {
          name = "global-cli-profile";
          paths = [
            pkgs.cloudflared
            pkgs.coreutils-prefixed
            db-clients
            pkgs.direnv
            emacs-weekly
            pkgs.fd
            pkgs.fzf
            pkgs.gh
            pkgs.git-lfs
            pkgs.gnupg
            pkgs.htop
            pkgs.jq
            pkgs.opam
            pkgs.pinentry_mac
            pkgs.ripgrep
            pkgs.tmux
            pkgs.tokei
            tree-sitter-grammars
            pkgs.tree
            pkgs.universal-ctags
            pkgs.uv
            pkgs.valkey
            pkgs.wget
            pkgs.nodejs_24
          ];
        };
      };
    };
}
