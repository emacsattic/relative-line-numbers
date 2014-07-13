# Relative line numbers

`relative-line-numbers` is an Emacs package that displays relative line numbers
on the left margin.

## Installation

The package is available in [MELPA](http://melpa.milkbox.net/).

If you have MELPA in `package-archives`, use

    M-x package-install RET relative-line-numbers RET

If you don't, open `relative-line-numbers.el` in Emacs and call
`package-install-from-buffer`.

## Usage

To toggle the mode in the current buffer:

    M-x relative-line-numbers-mode

To toggle the mode globally:

    M-x global-relative-line-numbers-mode

To turn it on automatically everywhere, add the following line to your Emacs
init file:

    (global-relative-line-numbers-mode)

Please note that this package *does not* rely on `linum`. Make sure `linum-mode`
is disabled before enabling `relative-line-numbers-mode`.

## Customization

### Faces

 * `relative-line-numbers` controls the way line numbers are displayed.
 * `relative-line-numbers-current-line` controls the way the current line
   indicator is displayed.

### Variables

 * `relative-line-numbers-format` is a function determining how the line numbers
   are formatted.
 * `relative-line-numbers-count-invisible-lines` controls whether or not
   invisible lines should be counted.
 * `relative-line-numbers-delay` controls the delay before updating the line
   numbers.
