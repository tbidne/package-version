<div align="center">

# Package Version


[![Hackage](https://img.shields.io/hackage/v/package-version)](https://hackage.haskell.org/package/package-version)
![haskell](https://img.shields.io/static/v1?label=&message=9.4&logo=haskell&logoColor=655889&labelColor=2f353e&color=655889)
[![BSD-3-Clause](https://img.shields.io/github/license/tbidne/package-version?color=blue)](https://opensource.org/licenses/BSD-3-Clause)

[![nix](https://img.shields.io/github/workflow/status/tbidne/package-version/nix/main?label=nix%209.2&&logo=nixos&logoColor=85c5e7&labelColor=2f353c)](https://github.com/tbidne/package-version/actions/workflows/nix.yaml)
[![cabal](https://img.shields.io/github/workflow/status/tbidne/package-version/cabal/main?label=cabal&logo=haskell&logoColor=655889&labelColor=2f353c)](https://github.com/tbidne/package-version/actions/workflows/cabal.yaml)
[![stack](https://img.shields.io/github/workflow/status/tbidne/package-version/stack/main?label=stack&logoColor=white&labelColor=2f353c)](https://github.com/tbidne/package-version/actions/workflows/stack.yaml)
[![style](https://img.shields.io/github/workflow/status/tbidne/package-version/style/main?label=style&logoColor=white&labelColor=2f353c)](https://github.com/tbidne/package-version/actions/workflows/style.yaml)

</div>

# Package Description

This package defines a `PackageVersion` type that represents [PVP](https://pvp.haskell.org/) version numbers. We provide functionality for reading these numbers from cabal files at both runtime and compile-time, the latter being useful when implementing `--version` options in executables. See the haddocks on hackage for more information.
