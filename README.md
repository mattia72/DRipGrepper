## DRipGrepper
Yet another [ripgrep](https://github.com/BurntSushi/ripgrep) GUI, written in Delphi Pascal with the goal to integrate in the Delphi IDE as an extension.

![Screenshot](./screenshots/DripGepper_Form.png)

## Features and Todos

### Main Window
- [x] search text, parse `rg` output
  - [x] --vimgrep parser
  - [x] error parser: if a line couldn't be parsed, then it is put in the first column
  - [x] --pretty parser matching lines with colorized match position
  - [x] --vimgrep --context NUM (or -A=NUM -B=NUM) parser
  - [ ] --invert-match parser
  - [ ] --stats parser eg. msgbox on end with search statistics
  - [x] no match parser, if nothing found (0 in 0)
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
    - [x] option helper form 
    - [ ] appropriate parameters will be filtered 
  - [x] quick buttons for
    - [x] --ignore-case
    - [x] --case-sensitive
    - [x] --word-regexp
    - [x] --fixed-strings
  - [x] --glob filter settings can be set in separate editbox
  - [x] --hidden, --no-ignore can be set in the form
  - [x] --pretty can be set in the form
  - [x] --context=NUM can be set in the form
  

### :rocket: Open with...
- [x] show configured editors to open matching file on matching position
   - [ ] on double click ?
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
  - [ ] DebugTrace switch
- [ ] configuration dialog

### Delphi Extension 
  - [x] dockable window
  - [x] high DPI Scaling 
  - [x] menu item in Tools (shortcut configurable in `DripExtension*.ini`)
  - [x] Default shortcut Shift+Alt+R ( :warning: conflict with GExpert/MMX Reverse Statement)
  - [x] click on matching file, opens file in the editor on position 
     - [ ] expand collapsed code in {$REGION}-s
  - [x] popup menu
  - [x] save window position
     - [x] save in layout
     - [x] load saved layout
  - [x] search selected text
     - [ ] multi line selection handling (in ini?)
  - [ ] search only in opened files/project files/project group
  - [x] delphi extension release

 
### Misc
- [x] screenshots in Readme.md
- [x] [scoop](https://scoop.sh) install
  - [x] standalone
  - [ ] delphi ide extension
- [ ] [chocolatey](https://chocolatey.org) install

## Installation

If you want to be up to date with the latest versions.
Install [Scoop](https://scoop.sh), and then you can install and update dripgrepper from the
[official bucket](https://github.com/mattia72/scoop) :cool:

```
scoop bucket add dripgrepper-bucket https://github.com/mattia72/scoop
scoop install dripgrepper
```

## Thanks
-  [ripgrep](https://github.com/BurntSushi/ripgrep)
-  [CnPack](https://www.cnpack.org)
-  [DDevExtensions](https://github.com/ahausladen/DDevExtensions)
-  [dprocess](https://stackoverflow.com/a/45029879/2923283): port from freepascal
-  [dzlib](https://sourceforge.net/p/dzlib/code/HEAD/tree)
-  [GExpert](https://www.gexperts.org/download)
-  [regexpr](https://regex.sorokin.engineer/en/latest/)
-  [UniSynEdit](https://sourceforge.net/projects/synedit)
-  [VirtualTreeView](https://github.com/TurboPack/VirtualTreeView)
