let
  githubTarball = owner: repo: rev:
    builtins.fetchTarball { url = "https://github.com/${owner}/${repo}/archive/${rev}.tar.gz"; };

  nixpkgsSrc = commit:
    githubTarball "NixOS" "nixpkgs" commit;

  pkgs = import (nixpkgsSrc "c708f2f92a5bd04b14f58c0d8ab9772d2bc41f96") { };
in with pkgs;
  mkShell {
    LOCALE_ARCHIVE_2_27 = "${glibcLocales}/lib/locale/locale-archive";
    buildInputs = [
      ruby
      nodejs
      bundler
      zlib
      xz.dev
    ];
  }
