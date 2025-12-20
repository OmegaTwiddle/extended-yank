# extended-yank

This emacs package provides the ability to read the rich format of the clipboard so that users can paste more information into emacs. Inspired by [citrus-lemon/emacs-macos-clipboard](https://github.com/citrus-lemon/emacs-macos-clipboard)

## Dependencies

### macOS

This package relies on shell calls to osascript and pandoc. osascript should be a native utility on your macOS device. To get pandoc:

```sh
brew install pandoc
```

### Windows

This package relies on shell calls to powershell and pandoc. Powershell should be installed by default on your Windows device. To get pandoc:

```powershell
choco install pandoc
```

Alternatively, head to https://pandoc.org/installing.html#windows for other options.

### Linux

This package relies on shell calls to xclip or wl-paste and pandoc. To get pandoc, use your package manager. For example, on Ubuntu:

For wayland:

```sh
sudo apt-get install pandoc wl-clipboard
```

For xorg:

```sh
sudo apt-get install pandoc xclip
```


If you use arch, btw, you can get the depencies this way:

For wayland:

```sh
sudo pacman -S pandoc wl-clipboard
```

For xorg:

```sh
sudo pacman -S pandoc xclip
```

## Installation

git clone this directory into your emacs.d directory, then add the following to your init file:

```elisp
(add-to-list 'load-path "/path/to/.emacs.d/extended-yank")
(require 'extended-yank)
```

## Usage

If you like, you can bind `extended-yank-yank-html` to a key in your init file. For example:

```elisp
(global-set-key (kbd "C-c y") 'extended-yank-yank-html)
```




