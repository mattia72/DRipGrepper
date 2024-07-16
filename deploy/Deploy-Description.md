<!--

Version:     v2.8.1-beta
PrevVersion: v2.8.0-beta

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

### :mag: Search Dialog
- [x] extension: search open files, the second approach works: 
    - https://stackoverflow.com/questions/78631692/how-to-get-list-of-opened-files-in-delphi-ide
- [x] extension: search projekt files
- [?] extension: search open projekt files
- [ ] invert match parser

## :warning: Bugs (marked with :white_check_mark:, if fixed)
- [x] dummy text in additional options, can't be deleted from command line
- [x] rg.exe of vs code doesn't support --pretty
- extension: search projekt files:
  - [x] too large project fails with ERROR_FILENAME_EXCED_RANGE(206), command slicing is necessary
  - [x] if large project has error it doesn't appear at the top
  - [x] show full, relative path doesn't work
  - [x] short cut doesn't work
