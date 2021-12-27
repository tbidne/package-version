{ compilerVersion ? "ghc8107"
}:

let
  pkgs = import (fetchTarball "https://github.com/NixOS/nixpkgs/archive/ac169ec6371f0d835542db654a65e0f2feb07838.tar.gz") { };
  compiler = pkgs.haskell.packages."${compilerVersion}";
in
pkgs.mkShell {
  buildInputs =
    [
      pkgs.cabal-install
      compiler.ghc
    ];
}
