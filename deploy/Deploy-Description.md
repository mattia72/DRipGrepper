<!--

Version:     v3.2.0-beta
PrevVersion: v3.1.0-beta

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

## Improvements 
Marked with checkmark if ready, else planned as next.
- [ ] own prettifier switcher as toolbar button
- [ ] replace 
- [ ] filter result
- [x] F3 find next 
<!-- #### :mag: Search Dialog -->

### Search Dialog
- [ ] extension: search open projekt files
- [x] --encoding

## :warning: Bugs 
Marked with checkmark if fixed, else it is known bug.
- [x] rg options helper grid has weird entries (before rg 14.0 parsing fails. Workaround update rg!)
- [ ] drip icon in menu has no transparent background
- [x] rg.exe not found if path surrounded with ""
- [x] non unicode characters causes exception. Workaround: --encoding=UTF8
- [x] can't delete additional options
- [ ] extension: Open with... opens active file in delphi, not the selected
- [ ] reopen search dlg after search with gui set parameters