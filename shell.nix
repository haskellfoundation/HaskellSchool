let
  githubTarball = owner: repo: rev:
    builtins.fetchTarball { url = "https://github.com/${owner}/${repo}/archive/${rev}.tar.gz"; };

  nixpkgsSrc = commit:
    githubTarball "NixOS" "nixpkgs" commit;

  # NixOS 22.05 as of 2022-06-09
  pkgs = import (nixpkgsSrc "4348fc64bffc9687572125889ec15a47f3a7edca") { };

in with pkgs;
  mkShell {
    LOCALE_ARCHIVE_2_27 = "${glibcLocales}/lib/locale/locale-archive";
    buildInputs = [
      git
      ruby
      nodejs
      bundler
      zlib
      xz.dev
    ];
  }
