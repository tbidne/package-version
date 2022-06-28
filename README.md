<div align="center">

# Package Version


[![Hackage](https://img.shields.io/hackage/v/package-version)](https://hackage.haskell.org/package/package-version)
[![BSD-3-Clause](https://img.shields.io/github/license/tbidne/package-version?color=blue)](https://opensource.org/licenses/BSD-3-Clause)

[![nix](https://img.shields.io/github/workflow/status/tbidne/package-version/nix/main?label=nix&logo=nixos&logoColor=85c5e7&labelColor=2f353c)](https://github.com/tbidne/package-version/actions/workflows/nix_ci.yaml)
[![stack](https://img.shields.io/github/workflow/status/tbidne/package-version/stack/main?label=stack%2019.11&logoColor=white&labelColor=2f353c)](https://github.com/tbidne/package-version/actions/workflows/stack_ci.yaml)
[![style](https://img.shields.io/github/workflow/status/tbidne/package-version/style/main?label=style&logoColor=white&labelColor=2f353c)](https://github.com/tbidne/package-version/actions/workflows/style_ci.yaml)

[![8.10.7](https://img.shields.io/github/workflow/status/tbidne/package-version/8.10.7/main?label=8.10.7&logo=haskell&logoColor=904d8c&labelColor=2f353c)](https://github.com/tbidne/package-version/actions/workflows/ghc_8-10.yaml)
[![9.0.2](https://img.shields.io/github/workflow/status/tbidne/package-version/9.0.2/main?label=9.0.2&logo=haskell&logoColor=904d8c&labelColor=2f353c)](https://github.com/tbidne/package-version/actions/workflows/ghc_9-0.yaml)
[![9.2.1](https://img.shields.io/github/workflow/status/tbidne/package-version/9.2.1/main?label=9.2.1&logo=haskell&logoColor=904d8c&labelColor=2f353c)](https://github.com/tbidne/package-version/actions/workflows/ghc_9-2.yaml)

</div>

# Package Description

This package defines a `PackageVersion` type that represents [PVP](https://pvp.haskell.org/) version numbers. We provide functionality for reading these numbers from cabal files at both runtime and compile-time, the latter being useful when implementing `--version` options in executables. See the haddocks on hackage for more information.
