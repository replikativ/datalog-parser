# Change Log
All notable changes to this project will be documented in this file. This change log follows the conventions of [keepachangelog.com](http://keepachangelog.com/).

## [Unreleased]
### Added
- `:order-by` support, as used by Datahike and Datalevin, parsed into `Order`
  records in the `:qorder` field of the query
- `:having` and `:timeout` support, as used by Datalevin, parsed into
  `HavingPred` records in `:qhaving` and an integer in `:qtimeout`
- all of the above roundtrip through the unparser
- `datalog.analysis`, an opt-in namespace that reports the clauses of a query
  that no ordering can ever run, e.g. a predicate over a variable that nothing
  binds. Depends on nothing but the parser itself

### Fixed
- `:limit` and `:offset` given in map queries, e.g. `{:find ... :limit 5}`

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
