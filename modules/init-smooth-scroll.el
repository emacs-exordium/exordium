;;; Smooth scroll.
;;;
;;; This module implements smooth scrolling similar to OS X or iOS
;;; scrolling. It makes using a track pad on a macbook a lot easier.
;;; To enable it, edit ~/.emacs.d/prefs.el and add this line:
;;;
;;; (setq exordium-smooth-scroll t)
;;;
;;; Alternatively, you can enable it only on OS X using this line:
;;;
;;; (setq exordium-smooth-scroll exordium-osx)
;;;
;;; The scrolling speed is controlled by 2 constants: `smooth-scroll-weight'
;;; and `smooth-scroll-drift' (called weight and drift thereafter). Any scroll
;;; operation like page-up or page-down is divided into a sequence of `weight'
;;; + `drift' scroll steps which are progressively smaller. The first `weight'
;;; steps scroll N lines, with N decreasing step after step. The last `drift'
;;; steps consist of scrolling a single line each step.
;;;
;;; For example if `weight' = 3 `drift' = 2, scolling forward 20 lines will be
;;; performed using this sequence: (9 6 3 1 1) i.e. scroll 9 lines, then 6
;;; lines, etc. Notice that the sum is 20. `weight' defines the first 3 steps
;;; which may be greater than 1, and `drift' defines the last 2 steps, which
;;; are always 1.
;;;
;;; Credit: the algorithm is from the Sublimity plugin, but fine-tuned.
;;; See https://github.com/zk-phi/sublimity

(with-no-warnings (require 'cl))


;;; Customizable variables

(defcustom smooth-scroll-weight 10
  "Number of scroll steps with decreasing motion"
  :group 'exordium
  :type  'integer)

(defcustom smooth-scroll-drift 5
  "Number of scroll steps with very slow motion (1 line or column at a time)"
  :group 'exordium
  :type  'integer)

(defcustom smooth-scroll-criteria ()
  "User-defined list of conditions disabling smooth scroll. This should
be a list of sexp; if any of the sexps evaluates to nil, smooth
scrolling will be disabled. For example:
'((or (not (boundp 'cua--rectangle)) (not cua--rectangle))
  (or (not (boundp 'multiple-cursors-mode)) (not multiple-cursors-mode))
  (not (eq major-mode 'shell-mode)))"
  :group 'exordium
  :type  'sexp)


;;; Private variables

;;; Save modes
(defvar smooth-scroll-auto-hscroll-mode nil)
(defvar smooth-scroll-mouse-wheel-progressive-speed nil)

;;; Save things in pre-command hook
(defvar smooth-scroll-previous-line (line-number-at-pos (window-start)))
(defvar smooth-scroll-previous-col (window-hscroll))
(defvar smooth-scroll-previous-buffer (current-buffer))
(defvar smooth-scroll-previous-window (selected-window))


;;; Minor mode

(define-minor-mode smooth-scroll-mode
  "smooth-scrolling mode"
  ;; Initial value:
  nil
  ;; Indicator for mode line:
  nil ; "Smooth"
  ;; Key bindings:
  nil
  ;; Flags:
  :global t
  ;; body
  (cond (smooth-scroll-mode
         (setq smooth-scroll-auto-hscroll-mode auto-hscroll-mode
               smooth-scroll-mouse-wheel-progressive-speed mouse-wheel-progressive-speed)
         (setq auto-hscroll-mode nil
               mouse-wheel-progressive-speed nil)
         (add-hook 'pre-command-hook 'smooth-scroll-pre-command nil)
         (add-hook 'post-command-hook 'smooth-scroll-post-command t))
        (t
         (remove-hook 'pre-command-hook 'smooth-scroll-pre-command)
         (remove-hook 'post-command-hook 'smooth-scroll-post-command)
         (setq auto-hscroll-mode smooth-scroll-auto-hscroll-mode
               mouse-wheel-progressive-speed smooth-scroll-mouse-wheel-progressive-speed))))


;;; Animation functions

(defun smooth-scroll-speeds (amount)
  "Return a list of numbers, the sum of which being equal to
AMOUNT. AMOUNT is a positive or negative integer representing a
number of lines or columns to scroll to. The returned list
defines the different motion steps to execute in order to obtain
a smooth scrolling effect. This function uses the two constants
WEIGHT and DRIFT to calculate the list. The numer of elements in
the list is WEIGHT + DRIFT.
For example, if WEIGHT = 3, DRIFT = 2 and AMOUNT = 10, the result
is (4 2 1 1 1)"
  (cl-labels ((fix-list (lst &optional eax)
                (if (null lst)
                    nil
                  (let* ((rem (car lst))
                         (val (floor rem)))
                    (setq rem (+ (- rem val) (or eax 0)))
                    (setq val (if (>= rem 1) (1+ val) val))
                    (setq rem (if (>= rem 1) (1- rem) rem))
                    (cons val (fix-list (cdr lst) rem))))))
    (let (a lst)
      (cond ((< amount 0)
             (mapcar #'- (smooth-scroll-speeds (- amount))))
            ((< amount smooth-scroll-drift)
             (make-list amount 1))
            (t
             (setq amount (- amount smooth-scroll-drift))
             (setq a (/ (* 2 amount)
                        (+ (expt (float smooth-scroll-weight) 2)
                           smooth-scroll-weight)))
             (dotimes (n smooth-scroll-weight)
               (setq lst (cons (* a (1+ n)) lst)))
             (append (cl-remove-if #'zerop (sort (fix-list lst) #'>))
                     (make-list smooth-scroll-drift 1)))))))

(defun smooth-scroll-vscroll (lines)
  "Scroll vertically a number of LINES (positive or negative)
with animation"
  (cl-flet ((vscroll (l)
              ;; Scroll window vertically L lines (positive or negative)
              (goto-char (window-start))
              (forward-line l)
              (set-window-start nil (point))))
    (save-excursion
      (let ((speeds (smooth-scroll-speeds lines)))
        (vscroll (- lines))
        (dolist (speed speeds)
          (vscroll speed)
          (force-window-update (selected-window))
          (redisplay))))))

(defun smooth-scroll-hscroll (cols)
  "Scroll right or left a numer of COLS (positive or negative)
with animation"
  (cl-flet ((hscroll (c)
              ;; Scroll window horizontally C columns (positive or negative)
              (if (< c 0)
                  (scroll-right (- c))
                (scroll-left c))))
    (save-excursion
      (let ((speeds (smooth-scroll-speeds cols)))
        (hscroll (- cols))
        (dolist (speed speeds)
          (hscroll speed)
          (force-window-update (selected-window))
          (redisplay))))))

(defun smooth-scroll-horizontal-recenter ()
  (let ((cols (- (current-column)
                 (window-hscroll)
                 (/ (window-width) 2))))
    (if (< cols 0)
        (scroll-right (- cols))
      (scroll-left cols))))


;;; Hooks

(defun smooth-scroll-pre-command ()
  "Pre-command hook for smooth-scroll-mode"
  (setq smooth-scroll-previous-line (line-number-at-pos (window-start))
        smooth-scroll-previous-col (window-hscroll)
        smooth-scroll-previous-buffer (current-buffer)
        smooth-scroll-previous-window (selected-window)))

(defun smooth-scroll-post-command ()
  "Post-command hook for smooth-scroll-mode"
  (let ((do-scroll (and (eq smooth-scroll-previous-buffer (current-buffer))
                        (eq smooth-scroll-previous-window (selected-window))
                        (not (memq this-command '(scroll-bar-drag
                                                  scroll-bar-toolkit-scroll
                                                  scroll-bar-scroll-up
                                                  scroll-bar-scroll-down)))
                        (or (not smooth-scroll-criteria)
                            (cl-every 'eval smooth-scroll-criteria)))))
    (when do-scroll
      (let (deactivate-mark)
        ;; Vscroll
        (when (or (< (point) (window-start))
                  (>= (point) (window-end)))
          (recenter))
        ;; Hscroll
        (when (and smooth-scroll-auto-hscroll-mode
                   (or truncate-lines
                       (truncated-partial-width-window-p))
                   (or (< (current-column) (window-hscroll))
                       (< (+ (window-hscroll) (window-width))
                          (current-column))))
          (smooth-scroll-horizontal-recenter)))
      ;; Animation
      (let ((lines (- (line-number-at-pos (window-start))
                      smooth-scroll-previous-line))
            (cols (- (window-hscroll) smooth-scroll-previous-col)))
        (unless (zerop lines)
          (smooth-scroll-vscroll lines))
        (unless (zerop cols)
          (smooth-scroll-hscroll cols))))))


(provide 'init-smooth-scroll)
