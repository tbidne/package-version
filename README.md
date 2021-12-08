<div align="center">

# Package Version

[![Hackage](https://img.shields.io/hackage/v/package-version)](https://hackage.haskell.org/package/package-version)
[![Hackage Deps](https://img.shields.io/hackage-deps/v/package-version)](http://packdeps.haskellers.com/reverse/package-version)
[![Hackage CI](https://matrix.hackage.haskell.org/api/v2/packages/package-versionls/badge)](https://matrix.hackage.haskell.org/#/package/package-version)
[![Cabal CI](https://img.shields.io/github/workflow/status/tbidne/package-version/cabal/main?label=cabal&logoColor=white)](https://github.com/tbidne/package-version/actions/workflows/cabal_ci.yaml)
[![Stack CI](https://img.shields.io/github/workflow/status/tbidne/package-version/stack/main?label=stack&logoColor=white)](https://github.com/tbidne/package-version/actions/workflows/stack_ci.yaml)
[![Nix CI](https://img.shields.io/github/workflow/status/tbidne/package-version/nix/main?label=nix&logo=nixos&logoColor=white)](https://github.com/tbidne/package-version/actions/workflows/nix_ci.yaml)
[![MIT](https://img.shields.io/github/license/tbidne/package-version?color=blue)](https://opensource.org/licenses/MIT)

|                       | GHC | 9.2.1 | 9.0.1 | 8.10.7
|:---------------------:|:---:|:-----:|:-----:|:-----:
`package-version`       |     |       |       |
| [![`0.1.0.0`](https://img.shields.io/badge/release-0.1.0.0-blue)](https://github.com/tbidne/package-version/releases/tag/0.1.0.0) | | :heavy_check_mark: | :heavy_check_mark: | :heavy_check_mark:

</div>

# Package Description

This package can be used to retrieve a haskell package's version number (defined in the cabal file) at compile-time. This can be useful for implementing a `--version` option in executables.

## Types

First, we define a new type `PackageVersion` that wraps `Data.Version`, but:

1. `Version`'s `versionTags` is completely ignored by `PackageVersion`'s lawful instances (e.g. `Eq`, `Ord`, `Semigroup`).
1. Trailing zeroes in `versionBranch` are similarly ignored.

That is, we declare an equivalence class on the `versionBranch` up to trailing zeroes. In particular, the `Monoid` identity is

```haskell
[0] = { [], [0], [0,0], ... }
```

and its `Semigroup` instance takes the latest version (based on `Ord`).

## Functions

We can then retrieve the version at compile-time, e.g.

```haskell
>>> $$(packageVersionTH "package-version.cabal")
MkPackageVersion {unPackageVersion = Version {versionBranch = [0,1,0,0], versionTags = []}}

>>> $$(packageVersionStringTH "package-version.cabal")
"0.1.0.0"
```

The equivalent IO functions are also available:

```haskell
>>> packageVersionStringIO "package-version.cabal"
"0.1.0.0"

>>> packageVersionEitherIO "package-version.cabal"
Right (MkPackageVersion {unPackageVersion = Version {versionBranch = [0,1,0,0], versionTags = []}})
```

See `Package.Version` for more information.
