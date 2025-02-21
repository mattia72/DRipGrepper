<!--

Version:     v4.4.0-beta
PrevVersion: v4.3.0-beta

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
- [x] open with... labels for commands
- [ ] search for file names eg. rg.exe -g='*pattern*' --files <DIR>
- [ ] copy to clipboard for running in PowerShell or command prompt
  - [ ] PS needs & prefix and rg path and search text surrounded by ''
  - [ ] BAT needs rg path and search text surrounded by ""
- [ ] **extension**: display save all files message before search?
- [ ] **extension**: dearch only in opened project files
- [ ] save/load search histories to/from a file
- [ ] JSON config files
- [ ] Add a skin chooser feature? https://stackoverflow.com/questions/9906312/delphi-vcl-styles-tutorial-how-to-change-the-style-at-runtime

## :exclamation: Bugs
Marked with checkmark if fixed, else it is a known bug.
- [x] del key in filter edit
- [x] del key deletes hist item, even if it pushed in edit boxes
- [x] replace: 
    - [x] more match in same line replaces only the last occurrence even if user selects all match
    - [x] more match in same line replaces all occurrence, even if user selects only one
        - [x] replace by rg is ok: there will be changed every occurrence in every lines 
    - [x] replace by toolbar always ignores case
    - [x] the left arrow key in the replace edit control doesn't work
- [x] **extension**: shortcuts changed in config form, didn't changed in ini
- [ ] **extension**: if search only in current file, path of the file is not saved in history
- [ ] **extension**: drip icon in menu has no transparent background
- [ ] look on dpi change (drag to another monitor)
    - [x] search form switch between search and replace shrinks height
    - [ ] search form Use Regex button disappear if drag to another monitor
    - [ ] ugly config form 
