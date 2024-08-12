# Revision history for package-version

All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/),
and this project adheres to the [Haskell Package Versioning Policy](https://pvp.haskell.org/).

## [0.4] -- 2024-08-13
### Removed
* `Pretty` instances removed as these were not very useful.
* `Read` instance removed as it was of dubious utility (`fromText` is better).

### Changed
* Internal representation now uses `NonEmpty Word` instead of `[Int]`.
* Because the representation now preserves the desired invariants (non-empty
  and non-negative), the type is now fully exposed from the non-internal
  module. In particular, the original data constructor `UnsafePackageVersion`
  is replaced with `MkPackageVersion`.

### Added
* Updated bounds on `base`, `bytestring`, and `template-haskell`.
* Added `HasCallStack` to `unsafePackageVersion`.

## [0.3] -- 2022-10-21
### Changed
* Minimum length is now 1, not 2. This is due to cabal allowing package
  versions with a single digit.
* Accordingly, `mempty` is now `[0]`, not `[0,0]`.
* The `ValidationErrorTooShort` data constructor renamed to `ValidationErrorEmpty`.
* Field `unPackageVersion` removed as it allows unsafe construction via record
  update syntax. This is now an ordinary function.

### Added
* GHC 9.4 support.

## [0.2] -- 2022-07-25
### Changed
* Renamed error data constructors to improve consistency.
* Modified errors' `Exception` instances to use `pretty` over default `show`.

### Added
* Added `Generic` instance to `PackageVersion`.

### Fixed
* Used `bytestring` for reading file over bespoke function.

## [0.1.0.0] -- 2021-12-19

* First version. Released on an unsuspecting world.

[0.4]: https://github.com/tbidne/package-version/compare/0.3..0.4
[0.3]: https://github.com/tbidne/package-version/compare/0.2..0.3
[0.2]: https://github.com/tbidne/package-version/compare/0.1.0.0..0.2
[0.1.0.0]: https://github.com/tbidne/package-version/releases/tag/0.1.0.0
