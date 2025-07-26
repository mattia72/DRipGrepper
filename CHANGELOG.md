
# Changelog

All relevant changes to the project are documented in this file.

## [v4.10.1] - 2025-08-01

### Fixed
- '=' in search text is not interpreted in command line arguments (#15)
- handling empty collection of saved matches (#16)

### Changed
- Refactoring: The GitHub release functions (`New-Release`, `New-ReleaseNotes`, `Add-AssetToRelease`) were extracted from `Deploy-DRipGrepper.ps1` and implemented as reusable functions with explicit parameters in `GitHubReleaseUtils.ps1`.
- The calls in `Deploy-DRipGrepper.ps1` and `gh-release.ps1` were adapted accordingly, so that no global variables are needed anymore.

### Added
- This `CHANGELOG.md` file for better traceability of changes.

## [4.10.0] - 2025-07-24

### Added
- OpenInDelphi extension support for VS Code, allowing users to open Delphi files directly from the VSCode editor.

## [Previous Versions]
- See the release notes in the GitHub repository: https://github.com/mattia72/DRipGrepper/releases
