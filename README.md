<div align="center">

# Package Version


[![Hackage](https://img.shields.io/hackage/v/package-version)](https://hackage.haskell.org/package/package-version)
[![BSD-3-Clause](https://img.shields.io/github/license/tbidne/package-version?color=blue)](https://opensource.org/licenses/BSD-3-Clause)

[![nix](https://img.shields.io/github/workflow/status/tbidne/package-version/nix/main?label=nix&logo=nixos&logoColor=85c5e7&labelColor=2f353c)](https://github.com/tbidne/package-version/actions/workflows/nix_ci.yaml)
[![style](https://img.shields.io/github/workflow/status/tbidne/package-version/style/main?label=style&logoColor=white&labelColor=2f353c)](https://github.com/tbidne/package-version/actions/workflows/style_ci.yaml)

[![cabal 8.10](https://img.shields.io/github/workflow/status/tbidne/package-version/cabal_8-10/main?label=8.10&logo=haskell&logoColor=655889&labelColor=2f353c)](https://github.com/tbidne/package-version/actions/workflows/cabal_8-10.yaml)
[![cabal 9.0](https://img.shields.io/github/workflow/status/tbidne/package-version/cabal_9-0/main?label=9.0&logo=haskell&logoColor=655889&labelColor=2f353c)](https://github.com/tbidne/package-version/actions/workflows/cabal_9-0.yaml)
[![cabal 9.2](https://img.shields.io/github/workflow/status/tbidne/package-version/cabal_9-2/main?label=9.2&logo=haskell&logoColor=655889&labelColor=2f353c)](https://github.com/tbidne/package-version/actions/workflows/cabal_9-2.yaml)

[![stack lts-18](https://img.shields.io/github/workflow/status/tbidne/package-version/stack_lts-18/main?label=stack%20lts-18&logoColor=white&labelColor=2f353c)](https://github.com/tbidne/package-version/actions/workflows/stack_lts-18.yaml)
[![stack lts-19](https://img.shields.io/github/workflow/status/tbidne/package-version/stack_lts-19/main?label=stack%20lts-19&logoColor=white&labelColor=2f353c)](https://github.com/tbidne/package-version/actions/workflows/stack_lts-19.yaml)
[![stack nightly](https://img.shields.io/github/workflow/status/tbidne/package-version/stack_nightly/main?label=stack%20nightly&logoColor=white&labelColor=2f353c)](https://github.com/tbidne/package-version/actions/workflows/stack_nightly.yaml)

</div>

# Package Description

This package defines a `PackageVersion` type that represents [PVP](https://pvp.haskell.org/) version numbers. We provide functionality for reading these numbers from cabal files at both runtime and compile-time, the latter being useful when implementing `--version` options in executables. See the haddocks on hackage for more information.
