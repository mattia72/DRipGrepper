<!--

Version:     v2.8.0-beta
PrevVersion: v2.7.0-beta

Help Formatting:
https://docs.github.com/en/get-started/writing-on-github/getting-started-with-writing-and-formatting-on-github/basic-writing-and-formatting-syntax, 
https://github.com/ikatyang/emoji-cheat-sheet/blob/master/README.md)

### :mag: Search Dialog
# + new featuren
# + new feature
 
### :warning: Bug Fixes
#* bug

-->

## :rocket: Improvements (marked with :white_check_mark:, if ready)

### :droplet: Extension
- [x] icon on splash screen at start of the IDE
- [x] icon in about box of the IDE

### :mag: Search Dialog
- [x] extension: search current file
- [?] extension: search projekt files
    - [ ] too large project fails
- [?] extension: search open projekt files
- [x] prevent multiline text search 
- [ ] invert match

## :warning: Bugs (marked with :white_check_mark:, if fixed)
- [ ] dummy text in additional options, can't be deleted from command line
- [ ] what if too much projekt file are listed?
- [x] first open of search form doesn't update command line
- [x] the default search is case-sensitive; however, it is not set as rg parameter
- [x] extension: toolbar doesn't show all buttons