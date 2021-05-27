let pkgs = import (fetchTarball path) {};
    path = https://github.com/NixOS/nixpkgs/archive/master.tar.gz;
in with pkgs;
  mkShell {
    buildInputs = [
      ruby
      nodejs
      bundler
      zlib
    ];
  }
