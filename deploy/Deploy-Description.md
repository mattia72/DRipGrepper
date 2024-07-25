<!--

Version:     v2.9.0-beta
PrevVersion: v2.8.1-beta

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

## :rocket: Improvements 
Marked with checkmark if ready, else planned as next.

### :droplet: GUI

#### Main Window
- [x] own prettifier in case if rg.exe of vs code doesn't support --pretty
- [ ] own prettifier switcher as toolbar button
- [x] history item can be deleted
- [x] history item with colored statistic 

<!-- #### :mag: Search Dialog -->

### :droplet: Extension
- [x] drip icon in menu
- [ ] adding "insert as `uses`" in result context menu

#### :mag: Search Dialog
- [ ] extension: search open projekt files

## :warning: Bugs 
Checkmarked if fixed, else it is known bug.
- [x] path not saved in history
- [x] indent sometimes not working
- [ ] rg options helper grid has weird entries
- [ ] drip icon in menu has no transparent background
- [x] context causes exception
- [x] search after delete all history causes exception