# Changelog

## [v5.1.0-beta] - 2026-11-12

*In memory of Richard.*

### 💥 Added

- Regression tests for adjacent, repeated, Unicode, and zero-length JSON submatches, including lines with up to 100 matches.
- Tests for release archive versioning and selection of the latest build assets.

### 🔄 Changed

- Delphi 12 support is paused until someone sponsors a licence :). Deployment skips Delphi 12 builds and assets in the meantime.
- Release archive names now include the build number from each EXE or DLL.
- Deployment selects the newest archive for each binary type, architecture, and Delphi version, and removes older local archives.

### 🐞 Fixed

- Clicking Cancel in the Open With command editor no longer adds a new item to the command list.
- OK and Cancel buttons now align automatically with consistent spacing across inherited dialogs, including Open With and Search.
- JSON search results now include every match on a line, each with its own highlight and navigation column.
- Corrected UTF-8 byte-offset conversion for matches containing or following emoji.
- Reusing the JSON parser no longer retains results or error state from the previous line.

## [v5.0.0-beta] - 2026-11-07

*In memory of Richard.*

### 💥 Added

- Delphi 13 Florence support for the standalone app and IDE extension.
- File dates shown in application and extension information.
- The date format can be set in the settings form.
- Support for modification, creation, and last access timestamps in the results tree.
- Support for changing the date column's color and font in the UI.
- Drag and drop support for reordering history items in the search form.
- **extension** A warning icon is shown in the search form when a path is outside the IDE project, to avoid confusion about the search results.
- **extension** Errors and warnings can be indicated with different colors.
- File hints can be shown when hovering over files in the results tree, providing details such as the full path.
- Matching line hints can be shown when hovering over matching lines in the results tree, providing additional context.
- **extension** A summary is shown when there are too many search path entries, e.g. `86 paths (e.g. C:\...)`.
- A Clear log file button in the settings form.

### 🔄 Changed

- Removed the GExperts dependency; **Add to uses** now uses a built-in parser and undoable IDE edits.
- Updated icon handling and improved the search/replace layout.
- A shared base form provides consistent OK and Cancel buttons.
- Visible columns are aligned automatically on startup.
- Version updated to 5.0.0.

### 🐞 Fixed

- Relative-path handling and an exception caused by an empty filename.
- VirtualTreeView compatibility with Delphi 12/13 and Win64 unit-test compilation.
- **extension** Files were incorrectly reported as *outside of project scope*. Paths, including relative paths and paths using `/` separators, are now normalized before comparison. Project directories are matched only at directory boundaries, and project units (`ProjectFiles`, `ProjectFilesDirs`) are taken into account alongside the project directory and library path.
- **extension** The IDE context (active project, project units, library path) was cached for the entire IDE session. After switching the active project, the *outside of project scope* indicators and search path warning were still checked against the previous project. The cached context is now invalidated by an IDE notifier when the active project changes and checked again against the IDE, so the results view uses the new project's paths.
- Fixed the version update script.
- **extension** When the search form opened in the Delphi IDE, all text in every combo box was selected. Now, only the text in the active combo box is selected, so typing immediately replaces that entry.
- **extension** The search directory was reset to the previous Custom Location path when opening the search form a second time in a non-Custom IDE context (e.g. *All open files*, *Project files*), causing rg.exe to run against the wrong path. The context-driven search path is now preserved when the form is shown.
- CRLF line endings are now used consistently in all source files.
- Introduced a data module for centralized management of image resources.

## [v4.15.0-beta] - 2026-03-12

*In memory of Richard.*

### 💥 Added

- Output format settings: JSON or vimgrep (BREAKING CHANGE: history items saved by older versions cannot be loaded).
- Search form:
  - An expert mode toggle to show or hide advanced options.
  - Support for saving and loading the form's size and position.
  - **Regex Template Menu**: Manage and apply regex search patterns from a dropdown menu in the search form.
  - Version information shown when clicking the "*i*" icon in the bottom-right corner of the main form.
  - A new column for file modification dates in search results, with sorting support.

### 🔄 Changed

- Output options use a generalized OptionPanel.
- Settings management refactored and cleaned up across multiple classes.

### 🐞 Fixed

- Incorrect settings writes to the INI file caused by concurrent threads.
- The vimgrep and vimgrep pretty parsers now work correctly with the new output format settings.

## [v4.14.0-beta] - 2025-09-27

### 💥 Added

- `Open with...` and `Search with DripGrepper` can be invoked using IDE toolbar buttons.
  When customizing the toolbar in Delphi 12, the correct icons appear only after restarting the IDE.

### 🔄 Changed

- Redesigned the history loading and saving settings in the settings form.
- Switched to parsing JSON output from rg.exe instead of vimgrep output.

### 🐞 Fixed

- Exception when saving settings: DRipExtensions_OpenWith_Action_MenuItem already exists.
- History load and save settings were not saved correctly in some cases.

## [v4.13.0-beta] - 2025-09-12

### 💥 Added

- **extension**: Modified file detection and a save prompt before invoking `Open With...`.
- **extension**: Modified file detection and a save prompt before replacing text.
- **extension**: A new search context option: `Project Files Directories` (all directories containing project files).
- Unit tests for TArrayEx.GetFirstMatchIndex.

### 🔄 Changed

- Search form layout improvements.

### 🐞 Fixed

- A large gap between the "Search in" and "Output" group boxes in the search form (#26).
- The search context could become invalid after switching off expert mode.

## [v4.12.0-beta] - 2025-08-07

### 💥 Added

- **feature**: History support for the Filter and Replace text boxes in the search form.
  - Use the Up and Down arrow keys to navigate through history.
- **extension**: Support for searching library paths in expert mode.
- **extension**: Support for searching the project root directory.
- **extension**: New VSCode Open In Delphi commands to build or compile the active project.

### 🔄 Changed

- Unit tests for loading and saving history objects to streams.

### 🐞 Fixed

- Result filtering was disabled when RipGrepper started.
- An empty collection of saved matches caused an exception during deserialization (#16).
- Release information lines were incorrectly joined in the About tab of the settings form (#25).
- **extension**: Command-line arguments were not split correctly across lines in the search form's command-line viewer when a path contained spaces.

## [v4.11.0-beta] - 2025-08-04

### 💥 Added

- Added a Settings menu to the DRipExtensions menu.
- New unit tests for loading and saving history objects to streams.

### 🔄 Changed

- Faster loading of the settings form.
- ButtonEdit controls now have hints for the left and right buttons.
- Deployment script updated to build the correct unit test projects.
- The information message is no longer shown when the running version is newer than the current GitHub release.

### 🐞 Fixed

- An exception when opening the Extension Settings tab if no Delphi installation was found (#23).

## [v4.10.2-beta] - 2025-07-31

### 🐞 Fixed

- Disabled `Link with runtime packages` in the standalone version to avoid missing BPL errors (#19).
- Hid `Handle Open in Delphi commands` in the Extension tab for the standalone version (#20).

### 🔄 Changed

- Updated hints in the Extension tab.

## [v4.10.1-beta] - 2025-07-30

### 🐞 Fixed

- The '=' character in search text was not interpreted correctly in command-line arguments (#15).
- Handling of an empty collection of saved matches (#16).

### 🔄 Changed

- Refactoring: The GitHub release functions (`New-Release`, `New-ReleaseNotes`, `Add-AssetToRelease`) were extracted from `Deploy-DRipGrepper.ps1` and implemented as reusable functions with explicit parameters in `GitHubReleaseUtils.ps1`.
- Improved version parsing in the deployment script with better validation of the CHANGELOG.md format.
- **RESTComponent elimination**: Removed RESTComponent dependencies from the extension DLL to improve compatibility across Delphi versions (12.1 → 12.3).

### 💥 Added

- This `CHANGELOG.md` file for better traceability of changes.
- Automatic VSCode detection and configuration for the "Open With" functionality.

## [v4.10.0-beta] - 2025-07-24

### Added

- OpenInDelphi extension support for VSCode, allowing users to open Delphi files directly from the VSCode editor.

## [Previous Versions]

- See the release notes in the GitHub repository: https://github.com/mattia72/DRipGrepper/releases








