## DRipGrepper
Yet another [ripgrep](https://github.com/BurntSushi/ripgrep) GUI, written in Delphi Pascal with the goal to integrate in the Delphi IDE as an extension.

![Screenshot](./screenshots/04-02-2024_10-37-31.png)

## Features and Todos

### Main Window
- [x] search text, parse `rg` output
  - [x] --vimgrep parser
  - [x] error parser: if a line couldn't be parsed, then it is put in the first column
  - [ ] --pretty parser matching lines with colorized match position
  - [ ] --stats parser eg. msgbox on end with search statistics
  - [ ] no match parser
- [x] toolbar buttons to show/hide
  - [x] icons
  - [x] full/relative path
  - [x] alternate row colors
- [x] copy `rg` command line into clipboard
- [x] sorting by path and row
- [ ] sorting by column and text 
- [ ] grouping by path: only for limited match line.
- [ ] file handling (eg. open in explorer, copy, rename, delete)
- [ ] replace text in files --replace=TEXT

### :mag: Search Dialog
- [x] help parametrizing `rg` in search dialog
  - [x] filter help texts: can't be selected 
  - [ ] filter not appropriate parameters
  - [ ] quick buttons for
    - [ ] --ignore-case
    - [ ] --case-sensitive
    - [ ] --word-regexp
    - [ ] --fixed-strings
  - [ ] --glob settings as separate editbox

### :rocket: Open with...
- [x] show configured editors to open matching file on matching position
   - [x] on double click
   - [x] with menu / toolbar
- [ ] separate editor for distinct extensions

### Configure Open with...
![Screenshot](./screenshots/04-02-2024_11-04-47.png)

### Configuration
- [x] configuration is stored in ini file
  - [x] search text history saved
  - [x] search paths history saved
  - [x] `rg` parameter history saved
  - [x] view settings saved
    - [x] ShowFileIcon
    - [x] ShowRelativePath
    - [x] AlternateRowColors
    - [x] IndentLines
  - [ ] configurable length of history 
  - [x] *Open with...* settings saved
- [ ] configuration dialog

### Standalone
  - [x] standalone release
  - [x] high DPI Scaling 

### Delphi Extension https://github.com/mattia72/DRipGrepper/issues/1
  - [x] dockable window
  - [x] high DPI Scaling 
  - [x] menu item in Tools
  - [x] Default shortcut Shift+Alt+R
  - [ ] click on matching file, opens file in the editor on position 
  - [ ] search only in opened files/project files/project group
  - [ ] delphi extension release
 
### Misc
- [x] screenshots in Readme.md
- [ ] [scoop](https://scoop.sh) install
- [ ] [chocolatey](https://chocolatey.org) install

## Screenshots
![Screenshot](./screenshots/04-02-2024_10-37-31.png)

## Thanks
-  [ripgrep](https://github.com/BurntSushi/ripgrep)
-  [dprocess](https://stackoverflow.com/a/45029879/2923283): port from freepascal
-  [GExpert](https://www.gexperts.org/download)
-  [dzlib](https://sourceforge.net/p/dzlib/code/HEAD/tree)
-  [UniSynEdit](https://sourceforge.net/projects/synedit)
-  [regexpr](https://regex.sorokin.engineer/en/latest/)
-  [DDevExtensions]( https://github.com/ahausladen/DDevExtensions)
