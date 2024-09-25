<!--

Version:     v3.5.0-beta
PrevVersion: v3.4.1-beta

Help Formatting:
https://docs.github.com/en/get-started/writing-on-github/getting-started-with-writing-and-formatting-on-github/basic-writing-and-formatting-syntax, 
https://github.com/ikatyang/emoji-cheat-sheet/blob/master/README.md)

### :mag: Search Dialog
# + new feature
# + new feature
 
### :warning: Bug Fixes
#* bug

# TODO
# - Change Readme.md 
# - Change Deploy-Description.md 
# - Change file and product version in every projects for ALL CONFIGURATION!
# - Commit and push all changes
# - Run deploy script by pushing Ctrl+Shift+T in VSCode
-->

## :boom: Improvements 
Marked with checkmark if ready, else planned as next.
- [x] enhanced filter control 
- [ ] replace 
- [ ] save/load search histories in file
- [ ] config form
  - [ ] install as _extension_ in delphi ide
- [ ] json config files
- [ ] own prettifier switcher as toolbar button
- [ ] __extension__: search opened projekt files only

## :exclamation: Bugs 
Marked with checkmark if fixed, else it is known bug.
- [ ] `Additional Options` has to have saved defaults also
- [ ] drip icon in menu has no transparent background
- [ ] history item delete mit `DEL` key, deletes the first item
- [ ] history item open, set match case doesn't remove --ignore-case
- [x] _extension_ Exception:
```
    [471C512A]{DRipExtension.bpl} Ripgrepper.Ui.Middleframe.TRipGrepperMiddleFrame.ChangeHistoryNodeText + $6E
    [471C79BF]{DRipExtension.bpl} Ripgrepper.Ui.Middleframe.TRipGrepperMiddleFrame.GetNodeByIndex + $27
    [471C4FEF]{DRipExtension.bpl} Ripgrepper.Ui.Middleframe.TRipGrepperMiddleFrame.AddVstHistItem + $43
    [471C4E47]{DRipExtension.bpl} Ripgrepper.Ui.Middleframe.TRipGrepperMiddleFrame.AddOrUpdateHistoryItem + $7B
    [471C7BB9]{DRipExtension.bpl} Ripgrepper.Ui.Middleframe.TRipGrepperMiddleFrame.PrepareAndDoSearch + $5
    [471B7399]{DRipExtension.bpl} Ripgrepper.Ui.Topframe.TRipGrepperTopFrame.ActionSearchExecute + $21
    [471B8335]{DRipExtension.bpl} Ripgrepper.Ui.Topframe.TRipGrepperTopFrame.StartNewSearch + $D9
```
