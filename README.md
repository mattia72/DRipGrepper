## DRipGrepper
Yet another [ripgrep](https://github.com/BurntSushi/ripgrep) GUI, written in Delphi Pascal with the goal to integrate in the Delphi IDE as an extension.

![Screenshot](./screenshots/DripGepper_Form.png)

## Features and Todos

### Main Window
- [x] search text, parse `rg` output
  - [x] --vimgrep parser
  - [x] error parser: if a line couldn't be parsed, then it is put in the first column
  - [x] --pretty parser matching lines with colorized match position
  - [ ] --stats parser eg. msgbox on end with search statistics
  - [ ] no match parser
- [x] toolbar buttons to 
  - [x] expand / collapse tree
  - [x] show/hide icons
  - [x] full/relative path
  - [x] alternate row colors
- [x] copy `rg` command line into clipboard
- [x] sorting by path 
- [x] sorting by row
- [x] sorting by col
- [x] sorting by tex
- [x] grouping by path
- [ ] sorting groups by row / col / text
- [ ] file handling (eg. open in explorer, copy, rename, delete)
- [ ] replace text in files --replace=TEXT

### :mag: Search Dialog
- [x] help parametrizing `rg` in search dialog
  - [x] filter help texts: can't be selected 
  - [x] expert mode set in `DripGrepper.ini` makes rg options visible
    - [ ] appropriate parameters will be filtered 
  - [x] quick buttons for
    - [x] --ignore-case
    - [x] --case-sensitive
    - [x] --word-regexp
    - [x] --fixed-strings
  - [x] --glob settings as separate editbox

### :rocket: Open with...
- [x] show configured editors to open matching file on matching position
   - [ ] on double click
   - [x] with menu / toolbar
- [ ] separate editor for distinct extensions

### Configure Open with...
![Screenshot](./screenshots/04-02-2024_11-04-47.png)

### Configuration
- [x] configuration is stored in ini file (`DripGrepper.ini` for standalone and `DripExtension*.ini` for the extension)
  - [x] search text history saved
  - [x] search paths history saved
  - [x] `rg` parameter history saved
  - [x] view settings saved
    - [x] ShowFileIcon
    - [x] ShowRelativePath
    - [x] AlternateRowColors
    - [x] IndentLines
    - [x] ExpandNodes
  - [ ] configurable length of history 
  - [x] *Open with...* settings saved
- [ ] configuration dialog

### Standalone
  - [x] standalone release
  - [x] high DPI Scaling 

### Delphi Extension https://github.com/mattia72/DRipGrepper/issues/1
  - [x] dockable window
  - [x] high DPI Scaling 
  - [x] menu item in Tools (shortcut configurable in `DripExtension*.ini`)
  - [x] Default shortcut Shift+Alt+R
  - [x] click on matching file, opens file in the editor on position 
     - [ ] expand collapsed code in {$REGION}-s
  - [ ] popup menu
  - [ ] save window position
  - [ ] search word on cursor
  - [ ] search only in opened files/project files/project group
  - [ ] delphi extension release
 
### Misc
- [x] screenshots in Readme.md
- [ ] [scoop](https://scoop.sh) install
- [ ] [chocolatey](https://chocolatey.org) install

## Thanks
-  [ripgrep](https://github.com/BurntSushi/ripgrep)

-  [CnPack] (https://www.cnpack.org)
-  [DDevExtensions](https://github.com/ahausladen/DDevExtensions)
-  [dprocess](https://stackoverflow.com/a/45029879/2923283): port from freepascal
-  [dzlib](https://sourceforge.net/p/dzlib/code/HEAD/tree)
-  [GExpert](https://www.gexperts.org/download)
-  [regexpr](https://regex.sorokin.engineer/en/latest/)
-  [UniSynEdit](https://sourceforge.net/projects/synedit)
-  [VirtualTreeView](https://github.com/TurboPack/VirtualTreeView)
