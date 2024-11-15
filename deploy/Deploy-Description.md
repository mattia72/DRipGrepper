<!--

Version:     v3.8.2-beta
PrevVersion: v3.8.1-beta

Help Formatting:
https://docs.github.com/en/get-started/writing-on-github/getting-started-with-writing-and-formatting-on-github/basic-writing-and-formatting-syntax, 
https://github.com/ikatyang/emoji-cheat-sheet/blob/master/README.md)

### :mag: Search Dialog
# + new feature
# + new feature
 
### :warning: Bug Fixes
# * bug

# TODO
# - Update Readme.md 
# - Update Deploy-Description.md 
# - Update file and product version in every projects for ALL CONFIGURATION!
# - Commit and push all changes
# - Run deploy script by pushing Ctrl+Shift+T in VSCode
-->

## :boom: Improvements 
Marked with checkmark if ready, else planned ...
- [ ] **extension**: save all before search message?
- [ ] save/load search histories in file
- [x] config form
  - [ ] install as **extension** in Delphi IDE
- [ ] JSON config files
- [ ] **extension**: search opened project files only
- [ ] copy to clipboard for running in PowerShell or command prompt
- [x] win64 release
  - [x] sparate zip assets for platforms and extension
  - [ ] **extension** zip for separate delphi versions?

## :exclamation: Bugs
Marked with checkmark if fixed, else it is a known bug.
- [o] **extension**: replace doesn't replace files opened in buffer
- [ ] filter doesn't work
- [ ] **extension**: if search only in current file, path of the file is not saved in history
- [ ] **extension**: exception if there is no project opened 
- [ ] exception if rg.exe not found
- [ ] --hidden switch is on sometimes if opened from history list TODO: check empty _sParamvalue
- [x] **extension**: add to uses list popup menu sometimes not shown
- [ ] **extension**: drip icon in menu has no transparent background

