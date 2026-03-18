# Plan: Configurable Timestamp Columns

**TL;DR:** Extend the date column in the results tree to show not only "Last Modified" but also "Created" and "Last Accessed" file timestamps. The user selects which timestamp type to display via a setting in `AppSettingsForm`. The underlying `TVSFileNodeData` is extended with all three timestamps. This plan is a prerequisite for `plan-dateFilter.prompt.md`.

---

## Phase 1: Data Model (no UI dependencies)

1. **Add `EDateTimeType` enum** in `src/Common/RipGrepper.Common.SimpleTypes.pas`
   - `EDateTimeType = (dttLastWrite, dttCreation, dttLastAccess)`
   - Constants for display/persistence:
     - `DATE_TIME_TYPE_NAMES: array[EDateTimeType] of string = ('Last Modified', 'Created', 'Last Accessed')`
     - `DATE_TIME_TYPE_KEYS: array[EDateTimeType] of string = ('LastWrite', 'Creation', 'LastAccess')`

2. **Extend `TVSFileNodeData`** in `src/Common/RipGrepper.Common.NodeData.pas`
   - Add fields: `FileCreationTime: TDateTime`, `FileLastAccessTime: TDateTime`
   - Add method: `UpdateFileTime(_dateTimeType: EDateTimeType)` — loads only the requested timestamp (lazy)
     - `dttLastWrite` → `TFile.GetLastWriteTime()` (refactor existing `UpdateLastWriteTime`)
     - `dttCreation` → `TFile.GetCreationTime()`
     - `dttLastAccess` → `TFile.GetLastAccessTime()`
   - Add method: `GetFileTime(_dateTimeType: EDateTimeType): TDateTime` — returns the corresponding field
   - Keep existing `UpdateLastWriteTime` as wrapper calling `UpdateFileTime(dttLastWrite)` for backward compatibility
   - Initialize new fields to 0 in both `New()` overloads

3. **Extend `TNodeLookSettings`** in `src/Settings/RipGrepper.Settings.NodeLookSettings.pas`
   - Rename concept: `ShowLastModifiedDateColumn` → keep for backward compat, but add:
   - New setting: `FDateColumnType: IStringSetting` — persisted as `'LastWrite'` | `'Creation'` | `'LastAccess'`
   - Default: `'LastWrite'` (current behavior preserved)
   - Property: `DateColumnType: EDateTimeType` (getter/setter converts string ↔ enum)
   - Constant: `SETTING_DATE_COLUMN_TYPE = 'DateColumnType'`
   - Add to `VIEW_SETTINGS_TYPES` array

## Phase 2: AppSettingsForm UI (*depends on Phase 1*)

4. **Extend `AppSettingsForm`** in `src/UI/RipGrepper.UI.Settings.AppSettingsForm.pas` + `.dfm`
   - Add `cmbDateColumnType: TComboBox` below `cmbDateFormat`
   - Items: `'Last Modified'`, `'Created'`, `'Last Accessed'` (from `DATE_TIME_TYPE_NAMES`)
   - Label: `lblDateColumnType: TLabel` ('Date column shows:')
   - In `ReadSettings`: set `cmbDateColumnType.ItemIndex` from `FNodeLookSettings.DateColumnType`
   - In `WriteSettings`: save `cmbDateColumnType.ItemIndex` → `FNodeLookSettings.DateColumnType`
   - Position: below existing `cmbDateFormat` / `lblDateFormat` controls

## Phase 3: Column Rendering (*depends on Phase 1*)

5. **Update `VstResultGetText`** in `src/UI/RipGrepper.UI.MiddleFrame.pas` (COL_FILE_LAST_WRITE block)
   - Instead of always using `FileLastWriteTime`, use `GetFileTime(Settings.NodeLookSettings.DateColumnType)`
   - Call `UpdateFileTime(dateColumnType)` to ensure the selected timestamp is loaded

6. **Update `VstResultCompareNodes`** in `src/UI/RipGrepper.UI.MiddleFrame.pas` (COL_FILE_LAST_WRITE block)
   - Use `GetFileTime(Settings.NodeLookSettings.DateColumnType)` for sorting comparison

7. **Update `UpdateColumnVisibility`** in `src/UI/RipGrepper.UI.MiddleFrame.pas`
   - Set `ResolveFileLastWriteTime` based on `ShowLastModifiedDateColumn` (unchanged)
   - Update column header text to reflect selected type: `'Modified'` / `'Created'` / `'Accessed'`

8. **Update column header** in `src/UI/RipGrepper.UI.MiddleFrame.dfm` or initialization code
   - Dynamic column title based on `DateColumnType` setting

## Phase 4: Verification

9. **Unit test** for `TVSFileNodeData.UpdateFileTime` / `GetFileTime` — all three types
10. **Manual test**: change DateColumnType in AppSettings → column header and values update
11. **Manual test**: sort by date column with each timestamp type
12. **Regression test**: existing "Show Last Modified Date Column" toggle still works

---

## Relevant Files

- `src/Common/RipGrepper.Common.SimpleTypes.pas` — add `EDateTimeType` enum + constants
- `src/Common/RipGrepper.Common.NodeData.pas` — extend `TVSFileNodeData` with 2 new timestamp fields + methods
- `src/Settings/RipGrepper.Settings.NodeLookSettings.pas` — add `DateColumnType` setting
- `src/UI/RipGrepper.UI.Settings.AppSettingsForm.pas` + `.dfm` — add `cmbDateColumnType` dropdown
- `src/UI/RipGrepper.UI.MiddleFrame.pas` — update column rendering, sorting, header

## Decisions

- **Backward compatible**: `ShowLastModifiedDateColumn` toggling unchanged, new setting controls *which* timestamp
- **Lazy loading**: only the configured timestamp type is loaded per file (not all three)
- **Single column**: reuse existing COL_FILE_LAST_WRITE column, change its content/header — no new columns
- **Last Access Time caveat**: may return 0 on systems with `NtfsDisableLastAccessUpdate` — show as empty cell
- **Default**: `dttLastWrite` preserves current behavior for existing users
