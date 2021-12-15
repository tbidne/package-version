<div align="center">

# Package Version

[![GitHub release (latest SemVer)](https://img.shields.io/github/v/release/tbidne/package-version?sort=semver)](https://github.com/tbidne/package-version/releases/)
[![Hackage](https://img.shields.io/hackage/v/package-version)](https://hackage.haskell.org/package/package-version)
[![Hackage Deps](https://img.shields.io/hackage-deps/v/package-version)](http://packdeps.haskellers.com/reverse/package-version)
[![Hackage CI](https://matrix.hackage.haskell.org/api/v2/packages/package-versionls/badge)](https://matrix.hackage.haskell.org/#/package/package-version)
[![Cabal CI](https://img.shields.io/github/workflow/status/tbidne/package-version/cabal/main?label=cabal&logoColor=white)](https://github.com/tbidne/package-version/actions/workflows/cabal_ci.yaml)
[![Stack CI](https://img.shields.io/github/workflow/status/tbidne/package-version/stack/main?label=stack&logoColor=white)](https://github.com/tbidne/package-version/actions/workflows/stack_ci.yaml)
[![Nix CI](https://img.shields.io/github/workflow/status/tbidne/package-version/nix/main?label=nix&logo=nixos&logoColor=white)](https://github.com/tbidne/package-version/actions/workflows/nix_ci.yaml)
[![MIT](https://img.shields.io/github/license/tbidne/package-version?color=blue)](https://opensource.org/licenses/MIT)

</div>

# Package Description

This package defines a `PackageVersion` type that represents [PVP](https://pvp.haskell.org/) version numbers. We provide functionality for reading these numbers from cabal files at both runtime and compile-time, the latter being useful when implementing `--version` options in executables. See the haddock [here](https://tbidne.github.io/package-version/) or on hackage for more information.