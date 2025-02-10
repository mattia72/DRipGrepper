<!--

Version:     v4.3.0-beta
PrevVersion: v4.2.0-beta

Help Formatting:
https://docs.github.com/en/get-started/writing-on-github/getting-started-with-writing-and-formatting-on-github/basic-writing-and-formatting-syntax, 
https://github.com/ikatyang/emoji-cheat-sheet/blob/master/README.md)

# TODO on new release
# - Update Readme.md 
# - Update Deploy-Description.md 
# - Update file and product version in every projects for ALL CONFIGURATION!
# - Commit and push all changes
# - Run deploy script by pushing Ctrl+Shift+T in VSCode
-->

## :boom: Improvements 
Marked with checkmark if ready, else planned ...
- [ ] copy to clipboard for running in PowerShell or command prompt
  - [ ] ps needs & prefix and nrg path and search text surrounded by ''
  - [ ] bat needs rg path and search text surrounded by ""
- [ ] **extension**: save all before search message?
- [ ] **extension**: search opened project files only
- [ ] save/load search histories in file
- [ ] JSON config files
- [ ] skin chooser? https://stackoverflow.com/questions/9906312/delphi-vcl-styles-tutorial-how-to-change-the-style-at-runtime

## :exclamation: Bugs
Marked with checkmark if fixed, else it is a known bug.
- [ ] **extension**: if search only in current file, path of the file is not saved in history
- [ ] **extension**: drip icon in menu has no transparent background
- [ ] look on dpi change (drag to another monitor)
    - [x] search form switch between search and replace shrinks height
    - [ ] search form Use Regex button disappear if drag to another monitor
    - [ ] ugly config form 
- [ ] replace: 
    - [ ] more match in same line replaces all occurence, even if user selects only one
        - [x] replace by rg is ok: there will be changed every occurence in every lines 
    - [ ] replace by toolbar is always ignore case
    - [ ] left arrow in replace edit ctrl doesn't work
