My local copy has a subdir called "myemacs" that has the before/after .el files that I use.
When cloning from github into a .emacs.d simply do:

for i in myemacs/*
do
  ln -s $i .
done


Also note that to make things work for emacs 24.3.1 vs 24.4 I need to not use some features:

diff --git a/init.el b/init.el
index 95d5799..9b4e8ff 100644
--- a/init.el
+++ b/init.el
@@ -134,7 +134,7 @@ Check the warnings and messages buffers, or restart with --debug-init")
 (let ((package-pinned-packages (append '(
                                          (diminish                . "melpa-pinned")
                                          (highlight-symbol        . "melpa-pinned")
-                                         (magit                   . "melpa-pinned")
+;;                                         (magit                   . "melpa-pinned")
                                          (git-timemachine         . "melpa-pinned")
                                          (git-gutter              . "melpa-pinned")
                                          (git-gutter-fringe       . "melpa-pinned")
@@ -279,7 +279,7 @@ the .elc exists. Also discard .elc without corresponding .el"
 (update-progress-bar)

 (require 'init-dired)           ; enable dired+ and wdired permission editing
-(require 'init-git)             ; Magit and git gutter
+;;(require 'init-git)             ; Magit and git gutter
 ;;(require 'init-git-visit-diffs) ; visit diffs in successive narrowed buffers
 (require 'init-flb-mode)        ; frame-local buffers
