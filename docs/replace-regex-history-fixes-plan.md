# Replace Regex And History Fix Plan

Date: 2026-07-21

## Status

- Replace-layout restore fixed in `src/UI/RipGrepper.UI.SearchForm.pas`.
- Remaining open items from this plan: replace-text normalization and history-tree replace-text ownership.

## Scope

This plan covers three user-visible defects around replace mode:

1. Regex replace entries do not render correctly in the tree.
2. An empty replace text is shown as `''` instead of as empty.
3. Reopening a replace history item selects the Replace tab, but the replace combobox stays hidden.

## Current Findings

### 1. Empty replace text is intentionally converted to `''`

Relevant code paths:

- `TRipGrepperSearchDialogForm.SetReplaceText` in `src/UI/RipGrepper.UI.SearchForm.pas`
- `TRipGrepperSearchDialogForm.SetReplaceTextSetting` in `src/UI/RipGrepper.UI.SearchForm.pas`
- `TRipGrepperSearchDialogForm.UpdateCheckBoxesByGuiSearchParams` in `src/UI/RipGrepper.UI.SearchForm.pas`

Observation:

- The dialog currently stores an empty replacement as `QuotedStr('')` when replace mode is active.
- Some UI paths dequote that value again, but tree/history paths use the stored value directly.
- That makes the persistence/command-line representation leak into the UI.

### 2. History tree replace text is sourced from global settings instead of the history item/node

Relevant code paths:

- `TMiddleLeftFrame.AddOrUpdateHistoryItem` in `src/UI/RipGrepper.UI.MiddleLeftFrame.pas`
- `TMiddleLeftFrame.AddVstHistItem` in `src/UI/RipGrepper.UI.MiddleLeftFrame.pas`
- `TMiddleLeftFrame.AddVstReplaceNode` in `src/UI/RipGrepper.UI.MiddleLeftFrame.pas`
- `TMiddleLeftFrame.ChangeHistoryNodeText` in `src/UI/RipGrepper.UI.MiddleLeftFrame.pas`
- `TMiddleLeftFrame.ChangeVstReplaceNode` in `src/UI/RipGrepper.UI.MiddleLeftFrame.pas`
- `TMiddleLeftFrame.NodeDataFromStream` in `src/UI/RipGrepper.UI.MiddleLeftFrame.pas`

Observation:

- `AddVstReplaceNode` fills the child node from `Settings.LastReplaceText` instead of from the node/history item that is being rendered.
- That can show stale text, the wrong text from another search, or the encoded `''` sentinel.
- This is the strongest local explanation for both:
  - empty replacements being shown as `''`
  - non-empty replacements showing the wrong text in the tree

### 3. Replace tab state and replace control visibility are restored through different paths

Relevant code paths:

- `TRipGrepperSearchDialogForm.CopyProxyToCtrls` in `src/UI/RipGrepper.UI.SearchForm.pas`
- `TRipGrepperSearchDialogForm.FormShow` in `src/UI/RipGrepper.UI.SearchForm.pas`
- `TRipGrepperSearchDialogForm.TabControl1Change` in `src/UI/RipGrepper.UI.SearchForm.pas`
- `TRipGrepperSearchDialogForm.ShowReplaceCtrls` in `src/UI/RipGrepper.UI.SearchForm.pas`
- `TRipGrepperSearchDialogForm.AdjustLayout` in `src/UI/RipGrepper.UI.SearchForm.pas`

Observation:

- `CopyProxyToCtrls` restores `TabControl1.TabIndex` and `cmbReplaceText.Text`.
- Replace-control visibility is not owned by that same restore step; it is handled later through layout methods.
- The form can therefore open with the correct tab selected while the replace controls still reflect the previous visibility state.

Status:

- Fixed.
- `TabControl1Change`, `CopyProxyToCtrls`, and `UpdateCheckBoxesByGuiSearchParams` now use the same internal replace-layout sync path.
- That sync step applies tab selection, updates `sflReplace`, and runs layout recalculation before the dialog is shown.

## Repair Plan

### 1. Normalize replace-text representation

Goal:

- Keep UI/model state as plain text, including the empty string.
- Only encode for the ripgrep command-line boundary if encoding is actually required there.

Steps:

1. Audit all places that write replace text into settings, history objects, and virtual-tree node data.
2. Remove the `QuotedStr('')` special case from UI-facing state, or isolate it behind a dedicated encode/decode helper.
3. Keep `TGuiSearchTextParams.ReplaceText`, `THistoryItemObject.ReplaceText`, `TRipGrepperSettings.LastReplaceText`, and `TVSHistoryNodeData.ReplaceData.ReplaceText` as the user-entered value.
4. If ripgrep still needs a special encoded empty replacement, perform that translation only where `RG_PARAM_REGEX_REPLACE` is written to the command-line options.

Expected outcome:

- Empty replace text renders as empty everywhere in the UI.
- History serialization still preserves an intentional empty replacement.

### 2. Fix tree-node text ownership

Goal:

- Render each replace node from the history item/node it represents, never from the global current settings.

Steps:

1. Change `TMiddleLeftFrame.AddVstReplaceNode` to use the replace text already present in `_nodeData` or passed node data.
2. Ensure `AddOrUpdateHistoryItem` and `ChangeHistoryNodeText` populate node data from the current history object, not only from `Settings.LastReplaceText`.
3. Ensure `NodeDataFromStream` keeps using `hio.ReplaceText` as the source of truth for loaded items.
4. Review `ChangeVstReplaceNode` so refresh/repaint paths do not overwrite child-node text with a global value.

Expected outcome:

- Regex replace history nodes show the actual replacement text.
- Reopening older items no longer picks up the last replacement from a different search.

### 3. Unify replace-layout restoration on form open

Status:

- Completed.

Goal:

- The selected tab and replace-control visibility must always be synchronized before the dialog is shown.

Steps:

1. Introduce a single internal sync step that applies replace mode to:
   - `TabControl1.TabIndex`
   - `sflReplace` in `FSearchFormLayout`
   - `cmbReplaceText.Visible`
   - replace-related help/button visibility
   - top-panel/layout recalculation
2. Call that sync step during the initial restore path in `CopyProxyToCtrls` or immediately from `FormShow` after writing proxy values.
3. Keep `TabControl1Change` as the user-interaction entry point, but make it reuse the same sync method instead of being the only place that makes the layout consistent.

Expected outcome:

- Opening a replace history item shows both the Replace tab and the replace combobox.

### 4. Add focused regression coverage

Target tests:

1. Extend `UnitTest/RipGrepper.Data.HistoryItemObjectTest.pas` with cases that verify:
   - empty replace text survives persistence as empty UI text
   - regex replace text survives persistence unchanged
2. Add a focused test around history-node creation if there is an existing test seam for `TMiddleLeftFrame`; otherwise use a manual verification checklist.
3. Add a UI regression check for opening a replace history item and verifying replace controls are visible.

Note:

- The history object tests already contain relevant persistence coverage and are the cheapest place to lock down the value semantics.
- The visibility issue is more UI-driven, so it may remain a manual verification if there is no existing GUI test harness for the search dialog.

## Validation Checklist

### Manual scenarios

1. Create a replace history item with regex enabled and a non-empty replacement. Confirm the tree shows the replacement text, not the search text and not a stale value.
2. Create a replace history item with an empty replacement. Confirm the tree shows an empty value, not `''`.
3. Save and reload history from stream. Confirm both cases above still render correctly.
4. Open a replace history item from the tree. Confirm the Replace tab is selected and the replace combobox is visible immediately.
5. Switch between Search and Replace tabs after loading a history item. Confirm visibility and layout stay in sync.

### Code-level checks

1. Verify every UI render path reads a decoded/plain replace value.
2. Verify every tree-node creation/update path uses per-item data, not `Settings.LastReplaceText`.
3. Verify command-line generation still emits the correct `--replace` form for an intentional empty replacement.

## Suggested Implementation Order

1. Fix replace-text normalization at the model/UI boundary.
2. Fix history-tree node population to stop using global settings.
3. Fix form-open layout synchronization for replace mode.
4. Add regression tests and run the narrowest available checks.

## Risk Notes

1. Empty replacement handling may be coupled to command-line generation, so the encoding boundary must be moved carefully.
2. History-node rendering currently mixes persisted item data and mutable global settings. That coupling should be reduced rather than patched in one branch only.
3. The search dialog has multiple layout entry points. Any fix should centralize replace-layout synchronization to avoid future drift.