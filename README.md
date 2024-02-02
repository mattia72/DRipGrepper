## DRipGrepper
Yet another [ripgrep](https://github.com/BurntSushi/ripgrep) GUI written in Delphi Pascal with the goal to integrate it as a plugin into Delphi IDE.

## Features and todos
- [ ] delphi integration #1
- [x] search text, parse `rg` output
  - [x] --vimgrep parser
  - [ ] no match parser
  - [ ] --pretty parser
- [x] help parametrizing `rg` in search dialog
  - [ ] filter not appropriate parameters
  - [ ] quick buttons for
    - [ ] --ignore-case
    - [ ] --case-sensitive
    - [ ] --word-regexp
    - [ ] --fixed-strings
  - [ ] --glob as separate editbox
- [x] open a configurable editor with matching file on matching position
- [ ] screenshots in Readme.md
- [ ] scoop install
- [ ] chocolatey install

     
  ## Thanks
-  [ripgrep](https://github.com/BurntSushi/ripgrep)
-  [dprocess](https://stackoverflow.com/a/45029879/2923283): port from freepascal
-  [GExpert](https://www.gexperts.org/download): idea, code and IDE integration framework
-  [dzlib](https://sourceforge.net/p/dzlib/code/HEAD/tree)
-  [UniSynEdit](https://sourceforge.net/projects/synedit)
