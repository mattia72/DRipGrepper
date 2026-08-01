# Custom Location search path is not restored when reopening search form

**Status:** Analysis phase — awaiting fresh log for confirmation.
**Reported:** 2026-07-31
**Scope:** Extension (`DRipExtensions.D11.dll`, `DRipExtensions.D12.dll`) — Delphi 11 / Delphi 12 IDE.
**Related fix:** [SearchDirContextResetBug.md](SearchDirContextResetBug.md) (non-Custom context regression, already fixed).

## Symptom

**Scenario A: Custom → Custom**

1. Open the DripGrepper search form inside the Delphi IDE.
2. Select the `Custom location(s):` radio button.
3. Enter a path, e.g. `C:\MyPath`, and start the search.
4. Close the search form.
5. Reopen the search form.

**Expected:** Radio still on `Custom location(s):`, `cmbSearchDir.Text = C:\MyPath`.
**Actual:** `cmbSearchDir.Text` is empty or shows a different value.

## Relevant components

| File | Symbol | Role |
| ---- | ------ | ---- |
| `src/UI/RipGrepper.UI.SearchForm.pas` | `LoadNewSearchSettings` | Restores persisted settings + calls `UpdateRbExtensionItemIndex` |
| `src/UI/RipGrepper.UI.SearchForm.pas` | `LoadExtensionSearchSettings` | Called after `CopySettingsToCtrlProxy`; calls `UpdateRbExtensionItemIndex` a second time |
| `src/UI/RipGrepper.UI.SearchForm.pas` | `UpdateRbExtensionItemIndex` | Builds `TIDEContextValues` (uses `FCtrlProxy.SearchPath` for `dicCustomLocation`) |
| `src/UI/RipGrepper.UI.SearchForm.pas` | `UpdateCmbsOnIDEContextChange` | For `dicCustomLocation`: sets `FContextSearchPath := ''` and `cmbSearchDir.Text := FCtrlProxy.SearchPath` |
| `src/UI/RipGrepper.UI.SearchForm.pas` | `CopySettingsToCtrlProxy` | Sets `_ctrlProxy.SearchPath := _ctrlProxy.SearchPathHist.SafeItem[0]` for new searches |
| `src/UI/RipGrepper.UI.SearchForm.pas` | `WriteCtrlProxyToCtrls` (post-fix) | On FormShow: skips overwrite for non-Custom contexts, still calls `SetCmbSearchPathText(FCtrlProxy.SearchPath)` for `dicCustomLocation` |
| `src/UI/RipGrepper.UI.SearchForm.pas` | `StoreCmbHistorieItems` | On Search: stores current search path into `FSearchPathHistByContext[currentContext]` and (only for `dicCustomLocation`) into `FCtrlProxy.SearchPathHist[0]` |
| `src/UI/RipGrepper.UI.SearchForm.pas` | `CopyProxyToSettings` | On FormClose: persists `FSearchPathHistByContext[dicCustomLocation]` into `FSettings.SearchPathsHistory`; persists `FCtrlProxy.ExtensionContext` into `FIDEContext.Value` |
| `src/Settings/RipGrepper.Settings.ExtensionSettings.pas` | `GetCurrentIDEContext` | Reads `FIDEContext.Value` back into `FCurrentIDEContext.IDESearchContext` |
| `src/Settings/RipGrepper.Settings.RipGrepParameterSettings.pas` | `SearchPath` (persisted key `SearchPath`) | Set in `WriteCtrlsToRipGrepParametersSettings` to `cmbSearchDir.Text` for `dicCustomLocation` |

## Expected end-to-end flow (Custom → Custom)

### Save (search click + FormClose)

1. `WriteCtrlsToSettings` → `StoreCmbHistorieItems`:
   - `currentContext = dicCustomLocation`.
   - `cmbSearchDir.Enabled = True`, `FContextSearchPath` empty → `searchPathToStore := cmbSearchDir.Text` (`= AAA`).
   - `FSearchPathHistByContext.StorePathForContext(dicCustomLocation, AAA)` → history for that context starts with `AAA`.
   - `FCtrlProxy.SearchPathHist.InsertUnique(0, AAA)`.
2. `WriteCtrlsToRipGrepParametersSettings`:
   - `cmbSearchDir.Enabled = True` and text is not a display label → `FSettings.RipGrepParameters.SearchPath := AAA`.
3. `FormClose` → `CopyCtrlsToProxy` (does NOT touch `SearchPath`) → `CopyProxyToSettings`:
   - `FSettings.SearchPathsHistory.Value := GetMaxCountHistoryItems(FSearchPathHistByContext[dicCustomLocation])` → `[AAA, …]`.
   - `rgec.IDESearchContext := FCtrlProxy.ExtensionContext (= dicCustomLocation)` → `FIDEContext.Value = 6`.
4. `FSettings.StoreToPersister` + `UpdateFile` → ini contains:
   - `[DelphiExtensionSettings] IDEContext=6`
   - `[RipGrepperSearchSettings] SearchPathsHistory=AAA;…`
   - `[RipGrepSettings] SearchPath=AAA`

### Reopen

1. `Create` → `setExtensionContextPanel` → `AddItems` → creates radio buttons; the `dicCustomLocation` item is built with `values := ''` (from `TDelphiIDEContext.GetValueByContext` which returns `''` for `dicCustomLocation`).
2. `LoadInitialSearchSettings` → `LoadNewSearchSettings`:
   - `FSettings.ReadFile` + `LoadFromDict` → in-memory settings reflect persisted state.
   - `SetCmbSearchPathText(FSettings.RipGrepParameters.SearchPath)` → `cmbSearchDir.Text := AAA`, `FContextSearchPath := AAA`.
   - `cic := FSettings.SearchFormSettings.ExtensionSettings.CurrentIDEContext` → `IDESearchContext = dicCustomLocation`.
   - `UpdateRbExtensionItemIndex(dicCustomLocation)`:
     - `SetSelectedIDEContext(dicCustomLocation)` sets the radio index (`OnContextChange` is suppressed via `FbExtensionOptionsSkipClick`).
     - Builds `icv := TIDEContextValues.Create(dicCustomLocation, FCtrlProxy.SearchPath, False)` — **but at this point `FCtrlProxy.SearchPath` is still empty**, because `CopySettingsToCtrlProxy` has not run yet.
     - `UpdateCmbsOnIDEContextChange(icv)`:
       - `dicCustomLocation` branch: `cmbSearchDir.Enabled := True`; `FContextSearchPath := ''`; `SetComboItemsAndText(cmbSearchDir, FCtrlProxy.SearchPath = '', customLocationItems)` → **`cmbSearchDir.Text := ''`**, i.e. the `AAA` set two statements earlier is discarded.
       - Also writes an incomplete `TDelphiIDEContext` (only `IDESearchContext` set) into `FSettings.SearchFormSettings.ExtensionSettings.CurrentIDEContext` — matches Folgeschritt #3 of the earlier analysis.
3. `LoadInitialSearchSettings` continues → `CopySettingsToCtrlProxy`:
   - `_ctrlProxy.SearchPathHist := [AAA, …]`.
   - `_ctrlProxy.SearchPath := _ctrlProxy.SearchPathHist.SafeItem[0] = AAA`.
4. `SeedSearchPathHistoryByContext`:
   - `FSearchPathHistByContext.SetForContext(dicCustomLocation, FCtrlProxy.SearchPathHist)` → `[AAA, …]` ✅
   - All IDE contexts seeded empty.
5. `LoadExtensionSearchSettings` → `UpdateRbExtensionItemIndex(dic.IDESearchContext = dicCustomLocation)`:
   - This time `FCtrlProxy.SearchPath = AAA`, so `icv` carries `AAA`.
   - `UpdateCmbsOnIDEContextChange` `dicCustomLocation` branch: `SetComboItemsAndText(cmbSearchDir, AAA, customLocationItems)` → `cmbSearchDir.Text := AAA` ✅.
6. `FormShow` → `WriteCtrlProxyToCtrls` (post-fix):
   - `FCtrlProxy.ExtensionContext = dicCustomLocation` → `SetCmbSearchPathText(FCtrlProxy.SearchPath = AAA)` → `cmbSearchDir.Text := AAA` ✅.
7. `WriteCtrlsToRipGrepParametersSettings`: `cmbSearchDir.Enabled = True`, text `AAA` (not a display label) → `FSettings.RipGrepParameters.SearchPath := AAA` ✅.

On paper the reopen ends with `cmbSearchDir.Text = AAA`. The reported failure means one of the following invariants breaks.

## Suspected root causes (ranked by likelihood)

### 1. `FIDEContext` is not the last user selection when reopening

If the persisted `IDEContext` is not `dicCustomLocation` on reopen (e.g. Init default `dicActiveFile`, or an earlier code path overwrote it), then `UpdateRbExtensionItemIndex` selects the wrong radio button and `UpdateCmbsOnIDEContextChange` takes an IDE-context branch. In that branch, `cmbSearchDir` gets loaded from an IDE context path (or empty), never from `FCtrlProxy.SearchPath`.

Log evidence to look for:
- `TRipGrepperExtensionSettings.GetCurrentIDEContext` messages after `ReadFile` on reopen — the resolved `IDESearchContext` value.
- Ini file `[DelphiExtensionSettings] IDEContext=` after step 3 of the save (expected: `6`).

Likely trigger:
- `SetCurrentIDEContext` writes `Integer(dicCustomLocation) = 6` correctly, but `UpdateCmbsOnIDEContextChange` writes an **incomplete** `TDelphiIDEContext` back into `CurrentIDEContext` at the end of the function (only `IDESearchContext` is populated). This makes `GetCurrentIDEContext` re-enter `LoadFromIOTA` on the next access (`ActiveProject.IsEmpty`). If the incomplete write happens on FormClose after `CopyProxyToSettings` and before `StoreToPersister`, the persisted `IDEContext` should still be `6`, but there is a race with other consumers.

### 2. `_ctrlProxy.SearchPathHist.SafeItem[0]` is not `AAA` on reopen

If `SafeItem[0]` returns `''` when the history is empty (e.g. persistence was skipped), then `_ctrlProxy.SearchPath := ''` and the `dicCustomLocation` branch of `UpdateCmbsOnIDEContextChange` sets `cmbSearchDir.Text := ''`.

Log evidence to look for:
- `CopySettingsToCtrlProxy` `Proxy filled from Settings: … SearchPath: <value>` line — expected `SearchPath: AAA`.
- Ini `[RipGrepperSearchSettings]` block contents.

Likely triggers:
- `StoreCmbHistorieItems` `searchPathToStore` guard: if `cmbSearchDir.Text` is somehow marked as a display label at save time (should not happen for a real path), the store is skipped.
- The Search‐click path calls `WriteCtrlsToSettings` → `StoreCmbHistorieItems` **before** `ModalResult := mrOk` closes the form; then FormClose runs `CopyCtrlsToProxy` → `CopyProxyToSettings`. `CopyCtrlsToProxy` does NOT copy `cmbSearchDir.Text` into `FCtrlProxy.SearchPath`, so any change made after `WriteCtrlsToSettings` would be lost — but that is not part of the reproducer.

### 3. First `SetCmbSearchPathText(AAA)` in `LoadNewSearchSettings` is discarded

Even though step 5 above ultimately re-sets `cmbSearchDir.Text := AAA`, the intermediate state during step 2 is `''`. This is fragile: any early handler that reads `cmbSearchDir.Text` between step 2 and step 5 sees `''`. This is not the immediate cause of the visible symptom (the final state should still be correct), but is a latent bug that should be addressed.

## What the fresh log should show

Please capture and include these blocks from `docs/analysis/DRipExtensions.log`:

**During the initial Custom search (save phase):**

- `TRipGrepperSearchDialogForm.WriteCtrlsToSettings`
- `TRipGrepperSearchDialogForm.StoreCmbHistorieItems` — the `Storing path for context 6: …` line.
- `TRipGrepperSearchDialogForm.WriteCtrlsToRipGrepParametersSettings` — the `SearchPath=` line.
- `TRipGrepperSearchDialogForm.CopyProxyToSettings` — the `Saved N Custom Location paths …` line and `IDESearchContext = …` line.
- `TRipGrepperSettings.UpdateFile` (or equivalent write log).

**Ini file after save (before reopen):**

- `[DelphiExtensionSettings]` section: `IDEContext=` value.
- `[RipGrepSettings]` section: `SearchPath=` value.
- `[RipGrepperSearchSettings]` section: `SearchPathsHistory` values.

**During the reopen (load phase):**

- `TRipGrepperExtensionSettings.GetCurrentIDEContext` — resolved value.
- `TRipGrepperSearchDialogForm.LoadNewSearchSettings` — the two `cmbSearchDir=` lines around `UpdateRbExtensionItemIndex`.
- `TRipGrepperSearchDialogForm.UpdateCmbsOnIDEContextChange` — the `ExtensionContext=…, Value=…` line and, if `dicCustomLocation`, the `SearchPath=` line.
- `TRipGrepperSearchDialogForm.CopySettingsToCtrlProxy` — the `Proxy filled from Settings: … SearchPath: …` line.
- `TRipGrepperSearchDialogForm.LoadExtensionSearchSettings` and its `UpdateRbExtensionItemIndex` call.
- `TRipGrepperSearchDialogForm.WriteCtrlProxyToCtrls` — the `Loaded N items from context 6` line and, in the `dicCustomLocation` branch, `cmbSearchDir.Text=` from `SetCmbSearchPathText`.
- `TRipGrepperSearchDialogForm.WriteCtrlsToRipGrepParametersSettings` — the resulting `SearchPath=` line on FormShow.

## Candidate fixes (deferred until log confirms root cause)

- **F1 (Ordering fix):** In `LoadInitialSearchSettings`, call `CopySettingsToCtrlProxy` **before** the `UpdateRbExtensionItemIndex` inside `LoadNewSearchSettings`, so that `FCtrlProxy.SearchPath` is already populated when `UpdateCmbsOnIDEContextChange` fires for `dicCustomLocation`. Removes the spurious empty-Text intermediate state.
- **F2 (Preserve on incomplete write):** In `UpdateCmbsOnIDEContextChange`, read the current `CurrentIDEContext`, only modify `IDESearchContext`, and write back — instead of assigning a fresh `TDelphiIDEContext` with only `IDESearchContext` populated. Prevents the `LoadFromIOTA` reload side effect and possible persistence of a half-empty record. (Folgeschritt #3 of the earlier analysis.)
- **F3 (Fallback in `dicCustomLocation` branch):** In `UpdateCmbsOnIDEContextChange`, for `dicCustomLocation`, if `FCtrlProxy.SearchPath` is empty use `_icv.GetValue()` or the first history item as a fallback, so the branch never blanks a pre-populated `cmbSearchDir.Text`.
- **F4 (Symmetry in `CopyCtrlsToProxy`):** Always copy `cmbSearchDir.Text` into `FCtrlProxy.SearchPath` on save (only when the text is not a display label), so `FCtrlProxy.SearchPath` reflects the truth at FormClose time — useful defence-in-depth.

The right choice depends on which invariant the fresh log shows as broken; F1 + F2 are the safest combination if all three suspected root causes contribute.
