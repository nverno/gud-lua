# Interactively debug lua using debugger.lua and GUD

[![License: GPL v3](https://img.shields.io/badge/License-GPLv3-blue.svg)](https://www.gnu.org/licenses/gpl-3.0)

This package provides GUD support for debugging lua programs instrumented with
`dbg` breakpoints from [debugger.lua](https://github.com/slembcke/debugger.lua).


## Installing

### Install gud-lua.el from source

- Clone this repository
- Add the following to your emacs config

```elisp
(require "[cloned nverno/gud-lua]/gud-lua.el")
```
