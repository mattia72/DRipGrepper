<!--

Version:     v3.0.0-beta
PrevVersion: v2.9.0-beta

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
- [ ] own prettifier switcher as toolbar button
- [ ] replace 
- [ ] filter result
- [x] search success unicode char indicators (Ok:'✔' Nothing found:'⛒' Error:'⚠')
<!-- #### :mag: Search Dialog -->

### :droplet: Extension

#### :mag: Search Dialog
- [ ] extension: search open projekt files

## :warning: Bugs 
Checkmarked if fixed, else it is known bug.
- [ ] rg options helper grid has weird entries
- [ ] drip icon in menu has no transparent background
- [x] search unicode character: 
    - [x] parsing fails
    - [x] highlight fails
    - [x] tree doesn't show unicode
- [x] searching fixed text with regex chars (.*) won't highlighted