<!--

Version:     v4.7.0-beta
PrevVersion: v4.6.2-beta

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
## 💥 Improvements
Marked with a checkmark if ready; otherwise, it is planned.
- [x] file content checked before replace, so the content won't be corrupted if the file is changed after searching
- [x] about form with version update check
- [x] new settings for search history loading behaviour
- [ ] search for file names eg. `rg.exe -g='*pattern*' --files <DIR>`
- [ ] **extension**: display save all files message before search?
- [ ] **extension**: search only in opened project files
- [ ] JSON config files

## 🐞 Bugs
Marked with checkmark if fixed, else it is a known bug.
- [ ] **extension**: if search only in current file, path of the file is not saved in history
- [ ] **extension**: drip icon in menu has no transparent background
- [ ] look on dpi change (drag window to another monitor)
  - [ ] search form Use Regex button disappear if drag to another monitor
  - [ ] ugly config form 
- [ ] **extension**: change theme if only start page is open causes exception
