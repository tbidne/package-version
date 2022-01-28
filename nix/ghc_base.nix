{ compilerVersion }:

let
  pkgs = import (fetchTarball "https://github.com/NixOS/nixpkgs/archive/945ec499041db73043f745fad3b2a3a01e826081.tar.gz") { };
  compiler = pkgs.haskell.packages."${compilerVersion}";
in
pkgs.mkShell {
  buildInputs =
    [
      pkgs.cabal-install
      compiler.ghc
    ];
}
