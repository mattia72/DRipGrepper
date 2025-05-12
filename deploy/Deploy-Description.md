<!--

Version:     v4.6.1-beta
PrevVersion: v4.6.0-beta

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
Marked with a checkmark if ready; otherwise, it is planned.
- [ ] search for file names eg. `rg.exe -g='*pattern*' --files <DIR>`
- [ ] **extension**: display save all files message before search?
- [ ] **extension**: search only in opened project files
- [ ] JSON config files
- [ ] Add a skin chooser feature? https://stackoverflow.com/questions/9906312/delphi-vcl-styles-tutorial-how-to-change-the-style-at-runtime

## :exclamation: Bugs
Marked with checkmark if fixed, else it is a known bug.
- [ ] load histories at startup
  - [ ] load replace historie items not working properly
  - [x] when a loaded history item is modified (e.g., adding --stats), the change is not recognized during the first search attempt but works on the second attempt. 
  - [x] saved history item doesn't contain additional options (e.g., --stats). 
- [ ] **extension**: if search only in current file, path of the file is not saved in history
- [ ] **extension**: drip icon in menu has no transparent background
- [ ] look on dpi change (drag window to another monitor)
  - [ ] search form Use Regex button disappear if drag to another monitor
  - [ ] ugly config form 
