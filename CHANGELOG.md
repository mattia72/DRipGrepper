# Changelog

## [v4.14.0-beta] - 2025-09-27

### 💥 Added
- `Open with...` and `Search with DripGrepper` can be called from IDE Toolbar buttons.
  The only caveat is that while toolbar customization in Delphi 12 the IDE doesn't show the right icons, only after restarting the IDE.

### 🔄 Changed
- Parsing json instead of vimgrep output of rg.exe

### 🐞 Fixed
- Exception when saving settings: DRipExtensions_OpenWith_Action_MenuItem already exists. bereits.

## [v4.13.0-beta] - 2025-09-12

### 💥 Added
- **extension**: Modified file detection and asking for save before calling `Open With...`
- **extension**: Modified file detection and asking for save before replace
- **extension**: New search context choice: `Project Files Directories` (all dirs of all project files)
- Unittests for TArrayEx.GetFirstMatchIndex

### 🔄 Changed
- Searchform layout improvements

### 🐞 Fixed
- Large gap between "Search in" and "Output" group boxes in search form (#26)
- After expert mode is switched off search context could be invalid

## [v4.12.0-beta] - 2025-08-07

### 💥 Added
- **feature**: History support for Filter and Replace textboxes in the search form
  - Use UP/DOWN arrows to navigate through history
- **extension**: Support search in library paths (in expert mode)
- **extension**: Search in project root directory
- **extension**: New VSCode Open In Delphi commands: build or compile active project

### 🔄 Changed
- Unittests for Load/SaveStream of History Objects

### 🐞 Fixed
- Filter result is disabled on startup of RipGrepper
- Empty collection of saved matches causes exception during deserialization #16
- Release info lines are joined erroneously in About tab of Settings form #25
- **extension**: Command line arguments are not properly separated in lines, in command line viewer of search form, if path contains spaces

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







