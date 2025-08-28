# Changelog

## [v4.12.0-beta] - 2025-08-07

### 💥 Added
- Support search in library paths

### 🔄 Changed
- Unittests for Load/SaveStream of History Objects

### 🐞 Fixed
- Empty collection of saved matches causes exception during deserialization #16
- Release info lines are joined erroneously in About tab of Settings form #25
- Command line arguments are not properly separated in lines, in command line viewer of search form, if path contains spaces

## [v4.11.0-beta] - 2025-08-04

### 💥 Added
- Settings menu added to DRipExtensions menu
- New unittests of Load/SaveStream of History Objects

### 🔄 Changed
- Faster load of settings form 
- ButtonEdit controls has hints for left and right buttons
- Deploy script update to build the right unittest projects
- If newer release runs as the current github release, info message is not shown.

### 🐞 Fixed
- Exception while opening Extension Settings tab if no installed Delphi found. (#23)

## [v4.10.2-beta] - 2025-07-31

### 🐞 Fixed
- In standalone version `Link with runtime packages` should be false to avoid bpl not found error (#19)
- `Handle Open in Delphi commands` hidden in Extension Tab for Standalone Version (#20)

### 🔄 Changed
- Some hints on the Extension Tab

## [v4.10.1-beta] - 2025-07-30

### 🐞 Fixed
- '=' in search text is not interpreted in command line arguments (#15)
- handling empty collection of saved matches (#16)

### 🔄 Changed
- Refactoring: The GitHub release functions (`New-Release`, `New-ReleaseNotes`, `Add-AssetToRelease`) were extracted from `Deploy-DRipGrepper.ps1` and implemented as reusable functions with explicit parameters in `GitHubReleaseUtils.ps1`.
- Improved version parsing in deployment script with better validation for CHANGELOG.md format
- **RESTComponent elimination**: Removed RESTComponent dependencies from extension DLL to improve compatibility across different Delphi versions (12.1 → 12.3)

### 💥 Added
- This `CHANGELOG.md` file for better traceability of changes.
- Automatic VSCode detection and configuration for "Open With" functionality

## [v4.10.0-beta] - 2025-07-24

### Added
- OpenInDelphi extension support for VSCode, allowing users to open Delphi files directly from the VSCode editor.

## [Previous Versions]
- See the release notes in the GitHub repository: https://github.com/mattia72/DRipGrepper/releases






