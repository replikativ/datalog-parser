# Change Log
All notable changes to this project will be documented in this file. This change log follows the conventions of [keepachangelog.com](http://keepachangelog.com/).

## [0.2.22]
### Changed
- Release workflow switched to replikativ style
- Every merge to main creates a release
- patch-version corresponds to commit-count

## [0.1.11]
### Changed
- Allow for valid datascript rules to be parsed @cldwalker

### Removed
- `validate-vars`-fn

## [0.1.1] - 2019-05-10
### Changed
- Documentation on how to make the widgets.

### Removed
- `make-widget-sync` - we're all async, all the time.

### Fixed
- Fixed widget maker to keep working when daylight savings switches over.

## 0.1.0 - 2019-05-10
### Added
- Files from the new template.
- Widget maker public API - `make-widget-sync`.
