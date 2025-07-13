<!--

Version:     v4.8.0-beta
PrevVersion: v4.7.2-beta

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
## 💥 Improvements:
Marked with a checkmark if ready; otherwise, it is planned.
- [x] **extension**: Delphi 12.3 support
- [x] load results from file on startup 
- [x] delete results with one click on x icon
- [ ] search for file names eg. `rg.exe -g='*pattern*' --files <DIR>`
- [ ] **extension**: display save all files message before search?
- [ ] **extension**: predefined patterns for search type, function, definition, etc.
- [ ] **extension**: search only in opened project files
- [ ] JSON config files

## 🐞 Bugs:
Marked with checkmark if fixed, else it is a known bug.
- [ ] after theme change some controls are not redrawn
- [ ] **extension**: if search only in current file, path of the file is not saved in history
- [ ] **extension**: drip icon in menu has no transparent background
- [ ] look on dpi change (drag window to another monitor)
  - [ ] search form Use Regex button disappear if drag to another monitor
  - [ ] ugly config form 
- [ ] **extension**: change theme if only start page is open causes exception

## Tested with:
- Delphi 11.3
- Delphi 12.1 CE
- Delphi 12.3 Athen