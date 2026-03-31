# Encoding Checkbox/ComboBox Sync – Test Cases

> **Note:** The encoding option (checkbox + combobox) is only visible in expert mode.

## Invariant
- If combobox has a value → checkbox must be checked, combobox enabled
- If checkbox is unchecked → combobox must be empty and disabled
- Command line must reflect the UI state (no stale `--encoding`)

## Verification
After each test step, check:
1. Checkbox checked state
2. Combobox text and enabled state
3. Command line preview (memo) for `--encoding=<value>` presence

---

## A – Loading History Items (Expert Mode)

[x] ### A1: Open history item WITH encoding 
1. Switch to expert mode
2. Run a search with `--encoding=utf8` set
3. Close search dialog
4. Click history item to re-open (still in expert mode)
[x] **Expected:** checkbox checked, combobox shows `utf8`, combobox enabled

[x] ### A2: Open history item WITHOUT encoding
1. Switch to expert mode
2. Run a search without encoding set
3. Close search dialog
4. Click history item to re-open
[x] **Expected:** checkbox unchecked, combobox empty, combobox disabled

[x] ### A3: Alternate between history items with/without encoding
1. In expert mode, create history item H1 with `--encoding=utf8`
2. Create history item H2 without encoding
[x] Click H1 → verify checkbox checked, combobox `utf8`
[x] Click H2 → verify checkbox unchecked, combobox empty
[x] Click H1 again → verify checkbox checked, combobox `utf8`

### A4: Open expert history item after switching from normal to expert mode
1. Create history item with `--encoding=utf8` in expert mode
2. Switch to normal mode (confirm msgbox)
3. Click the history item
4. Switch back to expert mode
[ ] **Expected:** checkbox unchecked, combobox empty, combobox disabled

### A5: Open non-expert history item in expert mode
1. Create history item in normal mode (no encoding possible)
2. Switch to expert mode, click the history item
[ ] **Expected:** checkbox unchecked, combobox empty, combobox disabled

---

## B – User Interaction (Expert Mode)

### B1: Check checkbox, select encoding, run search
1. Switch to expert mode
2. Check the encoding checkbox
3. Select `utf8` from combobox
[ ] **Expected:** combobox enabled, command line contains `--encoding=utf8`
5. Run search, re-open history item
[ ] **Expected:** checkbox checked, combobox `utf8`

### B2: Uncheck checkbox after selecting encoding
1. In expert mode, check encoding checkbox, select `utf8`
2. Uncheck checkbox
[ ] **Expected:** combobox text cleared, combobox disabled, `--encoding` removed from command line

### B3: Clear combobox text manually
1. In expert mode, check encoding checkbox, select `utf8`
2. Clear combobox text (backspace to empty)
[ ] **Expected:** checkbox should uncheck, combobox disabled

### B4: Type encoding manually in combobox
1. In expert mode, check encoding checkbox
2. Type `windows-1252` in combobox (not from dropdown)
[ ] **Expected:** checkbox stays checked, command line contains `--encoding=windows-1252`

### B5: Change encoding value via dropdown
1. In expert mode, check encoding checkbox, select `utf8`
2. Change to `ascii` via dropdown
[ ] **Expected:** checkbox stays checked, combobox shows `ascii`, command line updated

---

## C – Persistence Round-Trips

### C1: Encoding survives app restart
1. Set `--encoding=utf8`, run search
2. Close app completely
3. Re-open app, open last history item
[ ] **Expected:** checkbox checked, combobox `utf8`

### C2: No-encoding survives app restart
1. Run search without encoding
2. Close app completely
3. Re-open app, open last history item
[ ] **Expected:** checkbox unchecked, combobox empty

### C3: Multiple history items survive restart
1. Create H1 with `--encoding=utf8`, H2 without encoding, H3 with `--encoding=ascii`
2. Close and re-open app
[ ] Verify each history item loads correct encoding state

---

## E – Switching from Expert to Normal Mode

> **Behavior:** Unchecking `cbExpertMode` triggers `ActionShowExpertOptionsExecute`, which shows an info msgbox
> ("Switching to normal mode resets expert options to their default values.") and then resets all
> expert-only options (encoding, `cmbOptions` text) to their default values.

### E1: Msgbox appears when switching from expert to normal
1. Switch to expert mode
2. Uncheck `cbExpertMode` checkbox
[ ] **Expected:** info msgbox with text "Switching to normal mode resets expert options to their default values." is shown
4. Dismiss msgbox
[ ] **Expected:** mode is now normal, expert controls hidden

### E2: Encoding cleared when switching from expert to normal
1. Switch to expert mode
2. Check encoding checkbox, select `utf8` in combobox
3. Uncheck `cbExpertMode`
4. Confirm msgbox
[ ] **Expected:** encoding checkbox unchecked, combobox empty and disabled, `--encoding` absent from command line

### E3: Expert options textbox (cmbOptions) cleared when switching to normal
1. Switch to expert mode
2. Type `--max-count=5 --no-messages` in `cmbOptions`
3. Uncheck `cbExpertMode`
4. Confirm msgbox
[ ] **Expected:** `cmbOptions` text is cleared, options absent from command line

### E4: Both encoding and cmbOptions cleared together
1. Switch to expert mode
2. Check encoding checkbox, select `ascii`; type `--max-count=10` in `cmbOptions`
3. Uncheck `cbExpertMode`
4. Confirm msgbox
[ ] **Expected:** encoding combobox empty, `cmbOptions` empty, command line contains neither `--encoding` nor `--max-count`

### E5: Switching back to expert after normal reset starts fresh
1. Execute E2 (encoding cleared by switching to normal)
2. Switch back to expert mode
[ ] **Expected:** encoding checkbox unchecked, combobox empty (values were cleared, not restored)

### E6: cbExpertMode focus after msgbox
1. Switch to expert mode
2. Uncheck `cbExpertMode`
3. Dismiss msgbox
[ ] **Expected:** focus is on `cbExpertMode` (as set by `cbExpertMode.SetFocus` in code)

### E7: No msgbox when switching from normal to expert
1. Ensure normal mode
2. Check `cbExpertMode`
[ ] **Expected:** no msgbox shown, expert controls appear immediately

---

## D – Edge Cases

### D1: First launch in expert mode (no history, no settings)
1. Delete/rename settings file
2. Launch app, switch to expert mode
[ ] **Expected:** checkbox unchecked, combobox empty, combobox disabled

### D2: Rapid switching between history items in expert mode
1. In expert mode, create several history items (some with encoding, some without)
2. Click quickly between them
[ ] **Expected:** final state matches last clicked item

### D3: Encoding set in expert options textbox (bypass checkbox)
1. In expert mode, type `--encoding=utf8` directly in the expert options combobox
[ ] **Expected:** observe whether checkbox/combobox sync (currently they may not – document behavior)
