<!--

Version:     v4.0.0-beta
PrevVersion: v3.9.0-beta

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
- [x] separate color for row and col numbers
- [x] config form
  - [x] Debug and Expert mode
  - [x] rg.exe path
  - [x] **extension**: install as expert dll into Delphi IDE
  - [x] configure **extension** shortcuts
  - [x] configure colors, load defaults
- [ ] **extension**: save all before search message?
- [ ] save/load search histories in file
- [ ] JSON config files
- [ ] **extension**: search opened project files only
- [ ] copy to clipboard for running in PowerShell or command prompt

## :exclamation: Bugs
Marked with checkmark if fixed, else it is a known bug.
- [ ] **extension**: if search only in current file, path of the file is not saved in history
- [ ] exception if rg.exe not found
- [ ] **extension**: drip icon in menu has no transparent background
- [ ] background color 'nothing' should be handled transparent
- [ ] replace: 
    - [ ] more match in same line replaces all occurence, even if user selects only one
        - [x] replace by rg is ok: there will be changed every occurence in every lines 
    - [ ] replace by toolbar is always ignore case
- [x] context line nums are in Col instead of Row column
- [x] include/exclude path doubles 
- [ ] exception on closing ide
- [ ] ugly dark mode

