<!--

Version:     v4.1.0-beta
PrevVersion: v4.0.0-beta

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
- [ ] JSON config files
- [ ] **extension**: search opened project files only
- [ ] copy to clipboard for running in PowerShell or command prompt

## :exclamation: Bugs
Marked with checkmark if fixed, else it is a known bug.
- [ ] **extension**: if search only in current file, path of the file is not saved in history
- [ ] exception if rg.exe not found
- [ ] **extension**: drip icon in menu has no transparent background
- [ ] background color 'nothing' should be handled transparent
- [x] replace: 
    - [ ] more match in same line replaces all occurence, even if user selects only one
        - [x] replace by rg is ok: there will be changed every occurence in every lines 
    - [ ] replace by toolbar is always ignore case
- [o] ugly dark mode. read this: https://github.com/checkdigits/delphidarkmode

