## DRipGrepper
Yet another [ripgrep](https://github.com/BurntSushi/ripgrep) GUI, written in Delphi Pascal with the goal to integrate it as an extension into the Delphi IDE.

## Features and todos

### Main Window
- [x] search text, parse `rg` output
  - [x] --vimgrep parser
  - [ ] no match parser
  - [ ] --pretty parser
- [x] toolbar buttons to show/hide
  - [x] icons
  - [x] full/relative path
  - [x] alternate row colors
- [x] copy `rg` command line into clipboard
- [x] open a configurable editor with matching file on matching position
   - [x] on double click
   - [ ] menu, toolbar
- [x] sorting by file and row
- [ ] grouping by path
  
### Search Dialog
- [x] help parametrizing `rg` in search dialog
  - [ ] filter not appropriate parameters
  - [ ] quick buttons for
    - [ ] --ignore-case
    - [ ] --case-sensitive
    - [ ] --word-regexp
    - [ ] --fixed-strings
  - [ ] --glob as separate editbox

### Other
- [ ] delphi IDE integration https://github.com/mattia72/DRipGrepper/issues/1
- [ ] standalone release
- [ ] delphi extension release
- [ ] screenshots in Readme.md
- [ ] scoop install
- [ ] chocolatey install

## Thanks
-  [ripgrep](https://github.com/BurntSushi/ripgrep)
-  [dprocess](https://stackoverflow.com/a/45029879/2923283): port from freepascal
-  [GExpert](https://www.gexperts.org/download): idea, code and IDE integration framework
-  [dzlib](https://sourceforge.net/p/dzlib/code/HEAD/tree)
-  [UniSynEdit](https://sourceforge.net/projects/synedit)
