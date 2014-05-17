# Goal

Build a kick-ass portable emacs configuration.

Put it in github, so that any improvement is synchronized with all my machines
(home and Bloomberg).

# Features

Modular. For example someone else should be able to clone it and set his/her
own color theme.

Languages supported:
* ELisp
* Clojure
* C++ (with good indexing e.g. CEDET)
* JavaScript, Python, Ruby.

Other modes:
* Markdown
* Org
* XML/XSD

# Testing

Should only target Emacs 24, but we can have a macro for Emacs 23 just in case.

Must work in:
* Emacs on OSX
* Emacs on Linux (VM and Bloomberg)
* emacs -nw
* Optional: Emacs on Solaris (and screw AIX)

# Implementation

Start from scratch (copy dot.emacs and dot.emacs.d for reference).

Add languages and modes one by one.

Everything should fit into a git repo.
