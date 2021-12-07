<div align="center">

# Package Version

<a href="https://github.com/tbidne/package-version/actions/workflows/cabal_ci.yaml">
  <img alt="GitHub" src="https://img.shields.io/github/workflow/status/tbidne/package-version/cabal/main?label=cabal&logoColor=white&style=plastic">
</a>
<a href="https://github.com/tbidne/package-version/actions/workflows/stack_ci.yaml">
  <img alt="GitHub" src="https://img.shields.io/github/workflow/status/tbidne/package-version/stack/main?label=stack&logoColor=white&style=plastic">
</a>
<a href="https://github.com/tbidne/package-version/actions/workflows/nix_ci.yaml">
  <img alt="GitHub" src="https://img.shields.io/github/workflow/status/tbidne/package-version/nix/main?label=nix&logo=nixos&logoColor=white&style=plastic">
</a>
<a href="https://github.com/tbidne/package-version/actions/workflows/style.yaml">
  <img alt="GitHub" src="https://img.shields.io/github/workflow/status/tbidne/package-version/nix/main?label=style&logoColor=white&style=plastic">
</a>
<img alt="GitHub" src="https://img.shields.io/github/license/tbidne/package-version?style=plastic&color=blue">

<table>
  <thead>
    <th><code>package-version</code> / GHC</th>
    <th>9.0.1</th>
    <th>8.10.7</th>
  </thead>
  <tr>
    <td><code>0.1.0.0</code></td>
    <td>
      <a href="https://github.com/tbidne/package-version/actions/workflows/ghc_9-0-1.yaml">
        <img alt="GitHub" src="https://img.shields.io/github/workflow/status/tbidne/package-version/9.0.1/main?label=9.0.1&logo=haskell&logoColor=white&style=plastic">
      </a>
    </td>
    <td>
      <a href="https://github.com/tbidne/package-version/actions/workflows/ghc_8-10-7.yaml">
        <img alt="GitHub" src="https://img.shields.io/github/workflow/status/tbidne/package-version/8.10.7/main?label=8.10.7&logo=haskell&logoColor=white&style=plastic">
      </a>
    </td>
  </tr>
</table>
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
