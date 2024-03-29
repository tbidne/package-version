<div align="center">

# Package Version


[![Hackage](https://img.shields.io/hackage/v/package-version)](https://hackage.haskell.org/package/package-version)
[![ci](http://img.shields.io/github/actions/workflow/status/tbidne/package-version/ci.yaml?branch=main)](https://github.com/tbidne/package-version/actions/workflows/ci.yaml)
[![BSD-3-Clause](https://img.shields.io/github/license/tbidne/package-version?color=blue)](https://opensource.org/licenses/BSD-3-Clause)

</div>

# Package Description

This package defines a `PackageVersion` type that represents [PVP](https://pvp.haskell.org/) version numbers. We provide functionality for reading these numbers from cabal files at both runtime and compile-time, the latter being useful when implementing `--version` options in executables. See the haddocks on hackage for more information.

## Alternatives

Reading the cabal version can also be achieved by utilizing the generated `Paths_<pkg-name>` module. For instance, for a package named `foo`, we first add the generated module to our list of modules in `foo.cabal`:

```cabal
cabal-version:      2.4
name:               foo
version:            0.1

...

library
  other-modules:   Paths_foo
  autogen-modules: Paths_foo
```

Then we can reference this in our code:

```haskell
import Data.List (intercalate)
import Data.Version (Version(versionBranch))
import Paths_foo qualified as Paths

vsn :: String
vsn = intercalate "." $ fmap show $ versionBranch Paths.version
```

See the [documentation](https://cabal.readthedocs.io/en/3.10/cabal-package.html#accessing-data-files-from-package-code) for more details.