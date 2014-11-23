# Code Organization

## Directories

The source code is in these directories:

* modules: contains the configuration's modules, which all start with "init-".
* elpa: packages loaded from melpa.
* extensions: third party packages not present in melpa, included in source code.

## init.el

[init.el](https://raw.github.com/philippe-grenet/dot.emacs/master/init.el) is
the entry point. It includes all other files using the ```require```
feature. Any "required" module file must be ending with a ```provide```
statement. The name of the file and the symbol in ```provide``` and
```require``` must all be identical, otherwise Emacs can't find it.
