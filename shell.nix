let pkgs = import (fetchTarball path) {};
    path = https://github.com/NixOS/nixpkgs/archive/master.tar.gz;
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
