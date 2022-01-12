let
  githubTarball = owner: repo: rev:
    builtins.fetchTarball { url = "https://github.com/${owner}/${repo}/archive/${rev}.tar.gz"; };

  nixpkgsSrc = commit:
    githubTarball "NixOS" "nixpkgs" commit;

  pkgs = import (nixpkgsSrc "5fd4f796b4210d691b1f89e1f29043d635cd20e0") { };
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
