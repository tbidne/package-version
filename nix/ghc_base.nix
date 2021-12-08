{ compilerVersion ? "ghc8107"
}:

let
  pkgs = import (fetchTarball "https://github.com/NixOS/nixpkgs/archive/c71f061c68ba8ce53471b767d5049cbd0f3d8490.tar.gz") { };
  compiler = pkgs.haskell.packages."${compilerVersion}";
in
pkgs.mkShell {
  buildInputs =
    [
      pkgs.cabal-install
      compiler.ghc
    ];
}
