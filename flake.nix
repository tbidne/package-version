{
  description = "A Haskell package for retrieving the PVP version from cabal files.";
  inputs.flake-parts.url = "github:hercules-ci/flake-parts";
  inputs.nix-hs-utils.url = "github:tbidne/nix-hs-utils";
  inputs.nixpkgs.url = "github:nixos/nixpkgs/nixos-unstable";
  outputs =
    inputs@{ flake-parts
    , nix-hs-utils
    , nixpkgs
    , self
    }:
    flake-parts.lib.mkFlake { inherit inputs; } {
      perSystem = { pkgs, ... }:
        let
          ghc-version = "ghc962";
          compiler = pkgs.haskell.packages."${ghc-version}".override {
            overrides = final: prev: {
              hedgehog = prev.hedgehog_1_3;
              hlint = prev.hlint_3_6_1;
              ormolu = prev.ormolu_0_7_1_0;
            };
          };
          mkPkg = returnShellEnv:
            nix-hs-utils.mkHaskellPkg {
              inherit compiler pkgs returnShellEnv;
              name = "package-version";
              root = ./.;
            };
          hsDirs = "src test";
        in
        {
          packages.default = mkPkg false;
          devShells.default = mkPkg true;

          apps = {
            format = nix-hs-utils.format {
              inherit compiler hsDirs pkgs;
            };
            lint = nix-hs-utils.lint {
              inherit compiler hsDirs pkgs;
            };
            lint-refactor = nix-hs-utils.lint-refactor {
              inherit compiler hsDirs pkgs;
            };
          };
        };
      systems = [
        "x86_64-darwin"
        "x86_64-linux"
      ];
    };
}
