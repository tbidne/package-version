<div align="center">

# PackageVersion

![cabal](https://github.com/tbidne/package-version/workflows/cabal/badge.svg?branch=main)
![stack](https://github.com/tbidne/package-version/workflows/stack/badge.svg?branch=main)
![nix](https://github.com/tbidne/package-version/workflows/nix/badge.svg?branch=main)
![haddock](https://github.com/tbidne/package-version/workflows/haddock/badge.svg?branch=main)
![style](https://github.com/tbidne/package-version/workflows/style/badge.svg?branch=main)

</div>

This package can be used to retrieve a haskell package's version number (defined in the cabal file) at compile-time.

## Type

First, we define a new type `PackageVersion` that wraps `Data.Version` but:

1. `Version`'s `versionTags` is completed ignored by `PackageVersion`'s lawful instances (e.g. `Eq`, `Ord`, `Semigroup`).
1. Trailing zeroes in `versionBranch` are similarly ignored.

That is, we declare an equivalence class on the `versionBranch` up to trailing zeroes. In particular, the `Monoid` identity is

```haskell
[0] = { [], [0], [0,0], ... }
```

and its `Semigroup` instance takes the latest version (based on `Ord`).

## Functions

We can retrieve the version at compile-time, e.g.

```haskell
>>> $$(packageVersionTH "package-version.cabal")
MkPackageVersion {unPackageVersion = Version {versionBranch = [0,1,0,0], versionTags = []}}

>>> $$(packageVersionStringTH "package-version.cabal")
"0.1.0.0"
```

See `Package.Version` for more information.