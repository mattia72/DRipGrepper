<!--

Version:     v3.5.1-beta
PrevVersion: v3.5.0-beta

Help Formatting:
https://docs.github.com/en/get-started/writing-on-github/getting-started-with-writing-and-formatting-on-github/basic-writing-and-formatting-syntax, 
https://github.com/ikatyang/emoji-cheat-sheet/blob/master/README.md)

### :mag: Search Dialog
# + new feature
# + new feature
 
### :warning: Bug Fixes
# * bug

# TODO
# - Change Readme.md 
# - Change Deploy-Description.md 
# - Change file and product version in every projects for ALL CONFIGURATION!
# - Commit and push all changes
# - Run deploy script by pushing Ctrl+Shift+T in VSCode
-->

## :boom: Improvements 
Marked with checkmark if ready, else planned ...
- [ ] replace 
  - [x] replace ctrl on toolbar
  - [x] different color to highlight replaced text 
  - [ ] save / save all buttons
  - [x] rg.exe -r option handling
- [ ] popumenu to filter on file names only
- [ ] __extension__: save all before search message?
- [ ] save/load search histories in file
- [ ] config form
  - [ ] install as _extension_ in delphi ide
- [ ] json config files
- [ ] own prettifier switcher as toolbar button
- [ ] __extension__: search opened projekt files only
- [ ] copy to desktop to run in powershell / command prompt

## :exclamation: Bugs 
Marked with checkmark if fixed, else it is known bug.
- [ ] `Additional Options` has to have saved defaults also
- [ ] drip icon in menu has no transparent background
- [x] history item delete mit `DEL` key, deletes the first item
- [ ] exception if rg.exe not found 
- [x] __extension__ : file name not visible
- [x] --hidden not updated 
- [x] if an opened history search item is changed to replace, results are empty. (after refresh will grid filled)
- [x] replace to empty string causes parser error if --pretty is set
- [x] __extension__ : Open with settings changed from menu will overwritten on closing IDE
