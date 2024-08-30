<!--

Version:     v3.3.0-beta
PrevVersion: v3.2.0-beta

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
- [ ] replace 
- [ ] filter result
- [ ] own prettifier switcher as toolbar button
- [ ] save search histories in file
- [x] hint on history items
- [x] `Open Search Form...`, `Copy Command Line`, popup menus on history items
- [x] path of rg.exe in vs code detected automatically
- [x] extension: new menu item for 'Open with active file in IDE'
    - [x] toolbar button opens active file in Delphi if nothing selected
<!-- #### :mag: Search Dialog -->

### Search Dialog
- [ ] extension: search opened projekt files only

## :warning: Bugs 
Marked with checkmark if fixed, else it is known bug.
- [x] extension: Open with... opens active file in delphi, not the selected
- [x] search whole word won't be highlighted 
- [x] search form init without file masks
- [x] extension: dblclik on history shouldn't search selected.
- [x] save as default doesn't save every combo boxes
  - [X] Extension: save as default causes exception
  - [ ] Additional Options has to have saved defaults also
  - [ ] Extension First open doesn't load defaults
- [x] Encoding is duplicated if switched from windows-xxx to utf8
- [x] open search form from history and change filemask duplicate -g parameters
- [x] Extension: refresh F5 disabled
- [ ] delete of some history entry causes exception 
- [ ] drip icon in menu has no transparent background
