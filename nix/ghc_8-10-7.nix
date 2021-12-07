{}:

let
  pkgs = import (fetchTarball "https://github.com/NixOS/nixpkgs/archive/c71f061c68ba8ce53471b767d5049cbd0f3d8490.tar.gz") { };
  compilerVersion = "ghc8107";
  compiler = pkgs.haskell.packages."${compilerVersion}";
in
compiler.developPackage {
  name = "package-version";
  root = ../.;
  returnShellEnv = true;
  modifier = drv:
    pkgs.haskell.lib.addBuildTools drv (with pkgs.haskellPackages; [
      cabal-install
      pkgs.zlib
    ]);
}
