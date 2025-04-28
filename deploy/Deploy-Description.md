<!--

Version:     v4.6.0-beta
PrevVersion: v4.5.1-beta

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
- [x] copy to clipboard for running in PowerShell or command prompt
  - [x] PS needs & prefix and rg path and search text surrounded by ''
  - [x] BAT needs rg path and search text surrounded by ""
  - [x] settings should be settable in config form 
- [ ] **extension**: display save all files message before search?
- [ ] **extension**: dearch only in opened project files
- [x] save/load search histories to/from a file
  - [ ] load histories at startup
- [x] combo box history item count settings
- [ ] JSON config files
- [ ] Add a skin chooser feature? https://stackoverflow.com/questions/9906312/delphi-vcl-styles-tutorial-how-to-change-the-style-at-runtime

## :exclamation: Bugs
Marked with checkmark if fixed, else it is a known bug.
- [ ] The ripgreppath value is incorrectly updated to the last used search path after performing a search operation
- [ ] replace doesn't work properly if history changed ( I clouldn't reproduce it yet )
- [x] replace doesn't work properly if there are more text to replace in same line 
- [ ] **extension**: if search only in current file, path of the file is not saved in history
- [ ] **extension**: drip icon in menu has no transparent background
- [ ] look on dpi change (drag window to another monitor)
    - [x] search form switch between search and replace shrinks height
    - [ ] search form Use Regex button disappear if drag to another monitor
    - [ ] ugly config form 
