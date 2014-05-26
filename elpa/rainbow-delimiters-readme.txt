Rainbow-delimiters is a “rainbow parentheses”-like mode which highlights
parentheses, brackets, and braces according to their depth. Each
successive level is highlighted in a different color. This makes it easy
to spot matching delimiters, orient yourself in the code, and tell which
statements are at a given level.

Great care has been taken to make this mode FAST. You shouldn't see
any discernible change in scrolling or editing speed while using it,
even in delimiter-rich languages like Clojure, Lisp, and Scheme.

Default colors are subtle, with the philosophy that syntax highlighting
shouldn't be visually intrusive. Color schemes are always a matter of
taste.  If you take the time to design a new color scheme, please share
(even a simple list of colors works) on the EmacsWiki page or via github.
EmacsWiki: http://www.emacswiki.org/emacs/RainbowDelimiters
Github: http://github.com/jlr/rainbow-delimiters


Installation:

1. Place rainbow-delimiters.el on your emacs load-path.

2. Compile the file (necessary for speed):
M-x byte-compile-file <location of rainbow-delimiters.el>

3. Add the following to your dot-emacs/init file:
(require 'rainbow-delimiters)

4. Activate the mode in your init file.
   You can choose to enable it only in certain modes, or Emacs-wide:

- To enable it only in certain modes, add lines like the following:
(add-hook 'clojure-mode-hook 'rainbow-delimiters-mode)

- To enable it in all programming-related emacs modes (Emacs 24+):
(add-hook 'prog-mode-hook 'rainbow-delimiters-mode)

- To activate the mode globally, add to your init file:
(global-rainbow-delimiters-mode)

- To temporarily activate rainbow-delimiters mode in an open buffer:
M-x rainbow-delimiters-mode

- To toggle global-rainbow-delimiters-mode:
M-x global-rainbow-delimiters-mode

Customization:

To customize various options, including the color scheme:
M-x customize-group rainbow-delimiters

deftheme / color-theme.el users:
You can specify custom colors by adding the appropriate faces to your theme.
- Faces take the form of:
  'rainbow-delimiters-depth-#-face' with # being the depth.
  Depth begins at 1, the outermost color.
  Faces exist for depths 1-9.
- The unmatched delimiter face (normally colored red) is:
  'rainbow-delimiters-unmatched-face'
