{ compilerVersion }:

let
  pkgs = import (fetchTarball "https://github.com/NixOS/nixpkgs/archive/5b091d4fbe3b7b7493c3b46fe0842e4b30ea24b3.tar.gz") { };
  compiler = pkgs.haskell.packages."${compilerVersion}";
in
pkgs.mkShell {
  buildInputs =
    [
      pkgs.cabal-install
      compiler.ghc
    ];
}
