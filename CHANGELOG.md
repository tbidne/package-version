# Revision history for package-version

All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/),
and this project adheres to the [Haskell Package Versioning Policy](https://pvp.haskell.org/).

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

[0.2]: https://github.com/tbidne/package-version/compare/0.1.0.0..0.2
[0.1.0.0]: https://github.com/tbidne/package-version/releases/tag/0.1.0.0