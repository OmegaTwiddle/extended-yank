# extended-yank

This emacs package provides the ability to read the rich format of the clipboard so that users can paste more information into emacs. Inspired by [https://github.com/citrus-lemon/emacs-macos-clipboard](citrus-lemon/emacs-macos-clipboard)

## Dependencies

### macOS

This package relies on shell calls to osascript and pandoc. osascript should be a native utility on your macOS device. To get pandoc:

`brew install pandoc`

### Windows

This package relies on shell calls to powershell and pandoc. Powershell should be installed by default on your Windows device. To get pandoc:

`choco install pandoc`

Alternatively, head to https://pandoc.org/installing.html#windows

### Linux

This package relies on shell calls to xclip or wl-paste and pandoc. To get pandoc, use your package manager. For example, on Ubuntu:

`sudo apt-get install pandoc`


If you use arch, btw, you can get it with:

`sudo pacman -S pandoc`

## Installation

git clone this directory into your emacs.d directory, then add the following to your init file:

```elisp
(add-to-list 'load-path "/path/to/.emacs.d/extended-yank")
(require 'extended-yank)
```

## 


