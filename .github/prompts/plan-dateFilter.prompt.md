# Plan: Date Filter for File Timestamps

**TL;DR:** Post-Search date filter that filters VirtualTreeView result nodes by a configurable file timestamp (Last Write, Creation, or Last Access time). Added as a third exclusive mode in `PopupMenuFilterMode` (alongside File/Text). When selected, opens a `TDateFilterForm` dialog to choose timestamp type and set From/To dates. The chosen date range is displayed in `edtFilter.Text` as a formatted string.

---

## Phase 1: Data Model & Settings (no UI dependencies)

1. **Extend `EFilterMode`** in `src/Common/RipGrepper.Common.SimpleTypes.pas`
   - Add `fmFilterDate` to the enum → participates in existing `TFilterModes` set

2. **Add `EDateTimeType` enum** in `src/Common/RipGrepper.Common.SimpleTypes.pas`
   - `EDateTimeType = (dttLastWrite, dttCreation, dttLastAccess)`
   - Determines which file timestamp is compared during date filtering

3. **Extend `TFilterSettings`** in `src/Settings/RipGrepper.Settings.NodeLook.FilterSettings.pas`
   - Add constant: `DATE_FILTER_MODE = 'Date'` (alongside `FILE_FILTER_MODE`, `TEXT_FILTER_MODE`)
   - Add `FILTER_MODES` array entry for `'Date'`
   - New settings: `FDateFrom: IStringSetting`, `FDateTo: IStringSetting`, `FDateTimeType: IStringSetting`
   - Properties:
     - `DateFrom: TDateTime`, `DateTo: TDateTime` (getter/setter converts string ↔ TDateTime)
     - `DateTimeType: EDateTimeType` (getter/setter converts string ↔ enum)
   - Dates persisted as ISO string (`yyyy-mm-dd hh:nn:ss`) in INI
   - DateTimeType persisted as string: `'LastWrite'` | `'Creation'` | `'LastAccess'`
   - Constants: `SETTING_DATE_FROM`, `SETTING_DATE_TO`, `SETTING_DATE_TIME_TYPE`
   - Extend `VIEW_SETTINGS` array with 3 new keys
   - Update `GetFilterModes`: when `ChosenFilterMode = 'Date'` → include `fmFilterDate`, exclude `fmFilterFile`+`fmFilterText`
   - Update `SetFilterModes`: when `fmFilterDate in Value` → `ChosenFilterMode := DATE_FILTER_MODE`
   - Update `Init`: create `FDateFrom`/`FDateTo`/`FDateTimeType` settings with defaults `''`/`''`/`'LastWrite'`

## Phase 2: Filter Logic (*depends on Phase 1*)

4. **Extend `TVSFileNodeData`** in `src/Common/RipGrepper.Common.NodeData.pas`
   - Add fields: `FileCreationTime: TDateTime`, `FileLastAccessTime: TDateTime`
   - Add method: `UpdateFileTime(_dateTimeType: EDateTimeType)` — loads only the requested timestamp (lazy)
     - `dttLastWrite` → `TFile.GetLastWriteTime()` (existing `UpdateLastWriteTime` refactored)
     - `dttCreation` → `TFile.GetCreationTime()`
     - `dttLastAccess` → `TFile.GetLastAccessTime()`
   - Add method: `GetFileTime(_dateTimeType: EDateTimeType): TDateTime` — returns the corresponding field
   - Keep existing `UpdateLastWriteTime` as wrapper for backward compatibility

5. **Add `FilterDateMode` method** in `src/UI/RipGrepper.UI.MiddleFrame.pas`
   - New method: `FilterDateMode(Node: PVirtualNode; _dateFrom, _dateTo: TDateTime; _dateTimeType: EDateTimeType)`
   - File-level nodes (Parent = RootNode):
     - Call `nodeData.UpdateFileTime(_dateTimeType)` to ensure timestamp is loaded
     - Get timestamp via `nodeData.GetFileTime(_dateTimeType)`
     - If `DateFrom > 0` and `timestamp < DateFrom` → filtered (hidden)
     - If `DateTo > 0` and `timestamp > DateTo` → filtered (hidden)
   - Child match nodes: inherit parent's filtered state

6. **Extend `FilterNodes` method** in `src/UI/RipGrepper.UI.MiddleFrame.pas`
   - Add `fmFilterDate` branch alongside existing `fmFilterFile`/`fmFilterText` branches:
     ```pascal
     end else if EFilterMode.fmFilterDate in _filterModes then begin
       FilterDateMode(Node, dateFrom, dateTo, dateTimeType);
     end;
     ```
   - Read DateFrom/DateTo/DateTimeType from `Settings.NodeLookSettings.FilterSettings` → no signature change needed

## Phase 3: UI — Filter Menu Entry & Date Filter Form (*parallel to Phase 2*)

7. **Add menu item to `PopupMenuFilterMode`** in `src/UI/RipGrepper.UI.TopFrame.dfm`
   - Add `miSetDateFilterMode: TMenuItem` as RadioItem (like `miSetFileFilterMode`/`miSetTextFilterMode`)
   - Action: `ActionSetDateFilterMode` (new TAction)
   - Position: after `miSetTextFilterMode`, before the separator `N1`

8. **Create `TDateFilterForm`** as new form `src/UI/RipGrepper.UI.DateFilterForm.pas` + `.dfm`
   - Modal dialog (follows `TConfigForm` pattern: create → ShowModal → free)
   - Controls:
     - **Timestamp type selector:**
       - `rgDateTimeType: TRadioGroup` with items: `'Last Modified'`, `'Created'`, `'Last Accessed'`
       - Maps to `EDateTimeType` (dttLastWrite, dttCreation, dttLastAccess)
     - **Date range:**
       - `lblDateFrom: TLabel` ('From:')
       - `dtpDateFrom: TDateTimePicker` (Format: `dtCustom`, FormatStr: `yyyy-mm-dd hh:nn:ss`)
       - `chkDateFrom: TCheckBox` ('Active') — enables/disables the From boundary
       - `lblDateTo: TLabel` ('To:')
       - `dtpDateTo: TDateTimePicker` (same format)
       - `chkDateTo: TCheckBox` ('Active') — enables/disables the To boundary
     - **Buttons:** `btnOk`, `btnCancel`, `btnClear`
   - Constructor takes `TFilterSettings` to load/store DateFrom/DateTo/DateTimeType
   - `btnOk`: writes all values to FilterSettings, ModalResult = mrOk
   - `btnClear`: resets dates to 0 and type to dttLastWrite, ModalResult = mrOk
   - `btnCancel`: ModalResult = mrCancel
   - **Note:** When `dttLastAccess` selected, show hint label: `'Last Access Time may be disabled on some systems'`

9. **Add `ActionSetDateFilterMode`** in `src/UI/RipGrepper.UI.TopFrame.pas`
   - `ActionSetDateFilterModeExecute`:
     ```pascal
     // Switch to date filter mode (exclusive with File/Text)
     SetFilterMode(EFilterMode.fmFilterFile, True);  // reset file mode
     SetFilterMode(EFilterMode.fmFilterText, True);   // reset text mode
     SetFilterMode(EFilterMode.fmFilterDate);          // activate date mode
     
     // Open date filter dialog
     var form := TDateFilterForm.Create(Settings.NodeLookSettings.FilterSettings);
     try
       if form.ShowModal = mrOk then begin
         // Update edtFilter display with date range
         UpdateFilterEditForDateMode();
         // Apply filter
         MainFrame.FilterNodes(edtFilter.Text, FFilterMode);
       end;
     finally
       form.Free;
     end;
     
     UpdateFilterEditMenuAndHint;
     Settings.StoreViewSettings(TFilterSettings.SETTING_FILTERMODE);
     ```

10. **Update `UpdateFilterEditMenuAndHint`** in `src/UI/RipGrepper.UI.TopFrame.pas`
   - Add date mode handling:
     - `ActionSetDateFilterMode.Checked := EFilterMode.fmFilterDate in FFilterMode`
     - When date mode active: set `edtFilter.ReadOnly := True` (dates only editable via dialog)
     - When file/text mode active: set `edtFilter.ReadOnly := False`
     - TextHint: `'Date Filter'` when date mode active
   - Disable CaseSensitive/UseRegex menu items when date mode active (irrelevant for dates)

11. **Add `UpdateFilterEditForDateMode` helper** in `src/UI/RipGrepper.UI.TopFrame.pas`
   - Reads DateFrom/DateTo/DateTimeType from FilterSettings
   - Formats display string in `edtFilter.Text`:
     - Prefix with timestamp type: `"[Modified]"`, `"[Created]"`, or `"[Accessed]"`
     - Both set: `"[Modified] 2025-01-01 08:00 - 2025-12-31 17:00"`
     - Only From: `"[Modified] ≥ 2025-01-01 08:00"`
     - Only To: `"[Modified] ≤ 2025-12-31 17:00"`
     - Neither: `""` (empty)

12. **Update `edtFilterRightButtonClick`** in `src/UI/RipGrepper.UI.TopFrame.pas`
    - When date mode active: re-open `TDateFilterForm` instead of toggling filter on/off
    - When file/text mode: existing behavior unchanged

## Phase 4: Integration & Persistence (*depends on Phase 1–3*)

13. **Extend `Initialize` in TopFrame**
    - Load `fmFilterDate` from settings into `FFilterMode`
    - If date mode active: call `UpdateFilterEditForDateMode()` to populate `edtFilter.Text`
    - Set `edtFilter.ReadOnly` accordingly

14. **Persist on change**
    - In `ActionSetDateFilterModeExecute`: `Settings.StoreViewSettings(SETTING_FILTERMODE)`
    - In `TDateFilterForm.btnOkClick`: `Settings.StoreViewSettings(SETTING_DATE_FROM)` + `SETTING_DATE_TO` + `SETTING_DATE_TIME_TYPE`

## Phase 5: Verification

15. **Unit test** for `FilterDateMode`:
    - Various timestamp values with each `EDateTimeType`
    - Edge cases: only From, only To, both, neither
    - Verify correct timestamp field is compared per type
16. **Unit test** for `TVSFileNodeData.GetFileTime` / `UpdateFileTime`
17. **Manual test**: switch between File → Text → Date modes, verify `edtFilter` behavior changes correctly
18. **Manual test**: switch between Last Modified / Created / Last Accessed in dialog, verify correct filtering

---

## UI Flow Summary

```
PopupMenuFilterMode
├── miSetFileFilterMode  (RadioItem) → edtFilter editable, TextHint="File Filter"
├── miSetTextFilterMode  (RadioItem) → edtFilter editable, TextHint="Text Filter"
├── miSetDateFilterMode  (RadioItem) → opens TDateFilterForm → edtFilter shows date range (ReadOnly)
├── ─── separator ───
├── miFilterModeCaseSensitive  (Checkbox, disabled in date mode)
└── miFilterModeUseRegex       (Checkbox, disabled in date mode)

edtFilter behavior per mode:
┌──────────┬────────────┬──────────────────────────────────────────┬──────────┐
│ Mode     │ ReadOnly   │ Text content                             │ RightBtn │
├──────────┼────────────┼──────────────────────────────────────────┼──────────┤
│ File     │ False      │ User types filename pattern              │ Toggle   │
│ Text     │ False      │ User types text/regex pattern            │ Toggle   │
│ Date     │ True       │ "[Modified] 2025-01-01 08:00 - 12-31"   │ Re-open  │
└──────────┴────────────┴──────────────────────────────────────────┴──────────┘
```

## Relevant Files

- `src/Common/RipGrepper.Common.SimpleTypes.pas` — add `fmFilterDate` + `EDateTimeType` enum
- `src/Common/RipGrepper.Common.NodeData.pas` — extend `TVSFileNodeData` with `FileCreationTime`, `FileLastAccessTime`, `UpdateFileTime`, `GetFileTime`
- `src/Settings/RipGrepper.Settings.NodeLook.FilterSettings.pas` — date settings (DateFrom, DateTo, DateTimeType) + persistence
- **`src/UI/RipGrepper.UI.DateFilterForm.pas` + `.dfm`** — new modal dialog with TDateTimePicker controls + timestamp type selector
- `src/UI/RipGrepper.UI.TopFrame.pas` + `.dfm` — menu item, action, UpdateFilterEditMenuAndHint changes
- `src/UI/RipGrepper.UI.MiddleFrame.pas` — `FilterDateMode` + `FilterNodes` extension

## Decisions

- **Post-search filter** (not ripgrep `--mtime`), uses file timestamps from `System.IOUtils.TFile`
- **Configurable timestamp type**: Last Write (default), Creation, or Last Access — selectable in dialog
- **Lazy timestamp loading**: only the selected timestamp type is loaded per file (not all three)
- **Third exclusive mode** in filter menu (Date is mutually exclusive with File/Text)
- **Date + time precision** (`TDateTimePicker` with `dtCustom`, format `yyyy-mm-dd hh:nn:ss`)
- **Modal dialog** for date entry (follows existing `TConfigForm` pattern)
- **edtFilter shows date range** as formatted read-only string with type prefix (`[Modified]`, `[Created]`, `[Accessed]`)
- **`TDateTime = 0` means "boundary open"** (no limit)
- **ISO date strings for INI** persistence
- **CaseSensitive/UseRegex disabled** in date mode (irrelevant)
- **Last Access Time caveat**: may be disabled on some Windows systems (`NtfsDisableLastAccessUpdate`) — show hint in dialog

## Open Points

1. **Icon for menu item**: reuse calendar-clock or add a dedicated one
2. **Keyboard shortcut**: optional Ctrl+D for date filter toggle — follow-up
3. **Quick presets**: consider adding preset buttons in the dialog (Today, Last 7 days, Last 30 days, This year) — follow-up
4. **Show timestamp column**: when date filter is active, auto-enable the "Show Last Modified Date Column" — or add columns for Creation/Access time too?
