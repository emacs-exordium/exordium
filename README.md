# What is this repo

A kick-ass portable emacs configuration, synchronized between all my machines.

# Features

Modular. For example someone else should be able to clone it and set his/her
own color theme.

Languages supported:
* ELisp
* TODO Clojure
* TODO C++ (with good indexing e.g. CEDET)
* TODO JavaScript
* TODO Python
* TODO Ruby.

Other modes:
* Markdown
* Org
* XML/XSD

# Testing

Should only target Emacs 24, but we can have a macro for Emacs 23 just in case.

Test plan:
* Emacs24 on OSX
* Emacs24 on Linux (VM and Bloomberg)
* emacs -nw
* Optional: Emacs on Solaris (and screw AIX)
