;;; ecb-html.el --- 

;; Copyright (C) 2001 Jesper Nordenberg

;; Author: Jesper Nordenberg <mayhem@home.se>

;; This program is free software; you can redistribute it and/or modify it under
;; the terms of the GNU General Public License as published by the Free Software
;; Foundation; either version 2, or (at your option) any later version.

;; This program is distributed in the hope that it will be useful, but WITHOUT
;; ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
;; FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more
;; details.

;; You should have received a copy of the GNU General Public License along with
;; GNU Emacs; see the file COPYING.  If not, write to the Free Software
;; Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.

;;; Commentary:

;; Generates the ECB website. This files generates seven files: main.html,
;; menu.html, top.html, logo.html links.html, all-news.html and
;; javabrowser.html. These files will be used by the start-file index.html and
;; all together build the complete website of ECB at
;; http://ecb.sourceforge.net.
;;
;; Do not change any html-file besides the index.html manually but do all
;; changes in this elisp file!

;; $Id$

;;; Code:

;; here a load-file is better because then we don´t need adding the html
;; subdir to the load-path.
(load-file "./html-helper.el")
(require 'ecb)

;; Colors
(setq h-body-bgcolor "#ffffff")

(setq h-section-title-bgcolor "#304080")
(setq h-section-title-fgcolor "#ffffff")
(setq h-section-text-bgcolor "#ffffff")
(setq h-section-text-fgcolor "#000000")

(defvar ecb-menu-color nil)
(setq ecb-menu-color "#cfcfff")
(defvar ecb-bullet nil)
(setq ecb-bullet "bullet.gif")

;; These shouldn't have to be changed
(defvar ecb-dirname nil)
(setq ecb-dirname (concat "ecb-" ecb-version))

(defvar ecb-latest-news nil
  "List of latest news displayed on the main page.")
(setq ecb-latest-news
      `(
        ,(h-sub-section "ECB 2.40 released! (2009-05-16)"
                        "ECB now requires full CEDET being installed (at least
                        1.0pre6). ECB has now more user-responsible
                        buffer-parsing based on the idle-mechanism of
                        semantic. In addition it fully supports current
                        semantic-analyzer for intellisense and type-finding. ECB is more stable and now fully compatible with Emacs 22 and 23
                        and also mostly with XEmacs. ECB is able to work with
                        indirect buffers if the base-buffer is filebased. It
                        has a complete reworked history-buffer which can be
                        bucketized and shows dead- and indirect-buffers in
                        different faces. It has new support for Git and
                        Monotone as version-control systems. In addition a lot
                        of bugs are fixed. "
                        (h-link "docs/Install-and-first-steps.html#Install%20and%20first%20steps" "Here") " is a short installation guide. "
                        "Click " (h-link "NEWS.html" "here")
                        " for information about all changes in the new version. ")
        ,(h-sub-section "ECB 2.32 released! (2005-07-11)"
                        "ECB offers now two new interactors (special ecb-windows): One for the semantic-analyser (of cedet) and one for displaying the definition of current symbol at point. In addition the up- and down-arrow-keys are also smart in the tree-buffers. Much better maximizing and minimizing of the ecb-windows. Support for (X)Emacs < 21 has been officialy removed. Full documentation of the library tree-buffer.el. Some important bug-fixes. "
                        (h-link "docs/Install-and-first-steps.html#Install%20and%20first%20steps" "Here") " is a short installation guide. "
                        "Click " (h-link "NEWS.html" "here")
                        " for information about changes in the new version. ")
        ,(h-sub-section "ECB 2.31 released! (2004-12-10)"
                        "This is mostly a bug-fix-release for native Windows-XEmacs. In addition beta-code for Clearcase-support has been added. It's strongly recommended to install this release when using native Window-XEmacs! "
                        (h-link "docs/Install-and-first-steps.html#Install%20and%20first%20steps" "Here") " is a short installation guide. "
                        "Click " (h-link "NEWS.html" "here")
                        " for information about changes in the new version. ")
        ))

(defvar ecb-rest-news nil
  "List of older news - these news are displayed in all-news.html ; see ;; ;; ;; ;; ;;
`ecb-html-all-news'.")
(setq ecb-rest-news
      `(
        ,(h-sub-section "ECB 2.30.1 released! (2004-12-01)"
                        "This is mostly a bug-fix-release which fixes the errors occured at load-time with ECB 2.30. In addition there are some enhancements to the VC-support introduced first with ECB 2.30. "
                        (h-link "docs/Install-and-first-steps.html#Install%20and%20first%20steps" "Here") " is a short installation guide. "
                        "Click " (h-link "NEWS.html" "here")
                        " for information about changes in the new version. ")
        ,(h-sub-section "ECB 2.30 released! (2004-11-26)"
                        "ECB can now display the version-control state of files in the tree-buffers; state is displayed with new image-icons. ECB is now capable of handling remote paths (e.g. TRAMP-, ANGE-FTP- or EFS-paths). Much better performance because all time consuming tasks (e.g. checking directories for emptyness or checking the VC-state) are performed stealthy. Now the current node in the methods-buffer can be expanded very precisely via new commands in the popup-menu. ECB has now the new upgrade policy \"Never touching the customization-files of a user without asking\". Some bug fixes. "
                        (h-link "docs/Install-and-first-steps.html#Install%20and%20first%20steps" "Here") " is a short installation guide. "
                        "Click " (h-link "NEWS.html" "here")
                        " for information about changes in the new version. ")
        ,(h-sub-section "ECB 2.27 released! (2004-08-31)"
                        "Much saver mechanism for resizing the permanent compile-window: Enlarging the compile-window does never destroy some ecb-windows, shrinking the compile-window shrinks always back to the correct size and all ecb-windows have their correct size after shrinking the compile-window. Some minor fixes. "
                        (h-link "docs/Install-and-first-steps.html#Install%20and%20first%20steps" "Here") " is a short installation guide. "
                        "Click " (h-link "NEWS.html" "here")
                        " for information about changes in the new version. ")
        ,(h-sub-section "ECB 2.26 released! (2004-08-11)"
                        "Improved erformance of the directories-buffer-display. Some new minor-features. Fixed an important bug with XEmacs concerning merging faces. Some minor fixes. "
                        (h-link "docs/Install-and-first-steps.html#Install%20and%20first%20steps" "Here") " is a short installation guide. "
                        "Click " (h-link "NEWS.html" "here")
                        " for information about changes in the new version. ")
        ,(h-sub-section "ECB 2.25 released! (2004-06-14)"
                        "Fixed one annoying bug which often resulted in an error \"stack-overflow error in equal\" when using hippie-expand (or maybe also in other situations). The history-buffer can now be sorted by extension too. Some more small fixes. "
                        (h-link "docs/Install-and-first-steps.html#Install%20and%20first%20steps" "Here") " is a short installation guide. "
                        "Click " (h-link "NEWS.html" "here")
                        " for information about changes in the new version. ")
        ,(h-sub-section "ECB 2.24 now available as XEmacs package 1.18! (2004-05-19)"
                        "The XEmacs-package ECB 1.18 can be installed either via "
                        (h-link "http://www.xemacs.org/Download/win32/setup.exe"
                                "XEmacs-netinstaller")
                        " (for Windows) or from " (h-link "ftp://ftp.xemacs.org:/pub/xemacs/packages/" "ftp.xemacs.org")
                        " or via the package-manager of XEmacs.")
        ,(h-sub-section "ECB 2.24 released! (2004-04-14)"
                        "New \"current-type\"-filter for the Methods-buffer. More and better icons for the Methods-buffer. Now directories are prescanned for emptyness so they are displayed as empty in the directories buffer. Fixed some bugs which made ECB 2.23 incompatible with semantic 1.4.X. "
                        (h-link "docs/Install-and-first-steps.html#Install%20and%20first%20steps" "Here") " is a short installation guide. "
                        "Click " (h-link "NEWS.html" "here")
                        " for information about changes in the new version. ")
        ,(h-sub-section "ECB 2.23 released! (2004-04-02)"
                        "Works with the new cedet1.0beta2-release which offers much better intellisense! The look&feel of the Methods-buffer now looks much nicer because it has been polished with a lot of new icons for methods, variables, classes and private, protected and public protection, static etc. (see Screenshot Nr. 1). New feature for applying default tag-filters for certain files. Distinction between functions and function-prototypes in the Methods-buffer. Better internal self-monitoring of ECB. Some important bug fixes "
                        (h-link "docs/Install-and-first-steps.html#Install%20and%20first%20steps" "Here") " is a short installation guide. "
                        "Click " (h-link "NEWS.html" "here")
                        " for information about changes in the new version. ")
        ,(h-sub-section "ECB 2.22 released! (2004-03-08)"
                        "New nifty feature for easy applying filters to the tags of the Methods-buffer. Much smarter mechanism to highlight the current tag in the methods-buffer. Some important bug fixes - some of them especially for users of native-Windows XEmacs. "
                        (h-link "docs/Install-and-first-steps.html#Install%20and%20first%20steps" "Here") " is a short installation guide. "
                        "Click " (h-link "NEWS.html" "here")
                        " for information about changes in the new version. ")
        ,(h-sub-section "ECB 2.21 released! (2004-02-16)"
                        "Better compatibility with other packages: Applications like Gnus, BBDB or Xrefactory run within the ECB-frame without conflicts - even when the ECB-windows are visible. The command `balance-windows' works now. Some important bug fixes. "
                        (h-link "docs/Install-and-first-steps.html#Install%20and%20first%20steps" "Here") " is a short installation guide. "
                        "Click " (h-link "NEWS.html" "here")
                        " for information about changes in the new version. ")
        ,(h-sub-section "ECB 2.11 released! (2003-11-14)"
                        "Semanticdb is used for jumping to external type-definitions. Special-display-buffers are handled correctly. Automatic upgrading has been fixed for this new version. Some bug fixes. "
                        (h-link "docs/Install-and-first-steps.html#Install%20and%20first%20steps" "Here") " is a short installation guide. "
                        "Click " (h-link "NEWS.html" "here")
                        " for information about changes in the new version. ")
        ,(h-sub-section "ECB 2.01 released! (2003-11-04)"
                        "New image-style tree-buffers. Complete overhaul of the popup-menu mechanism - now submenus are allowed and some new default entries. If the special ECB-windows are hidden then there are no restrictions about the window-layout of the ecb-frame. Runs with cedet 1.0. Some bug fixes. "
                        (h-link "docs/Install-and-first-steps.html#Install%20and%20first%20steps" "Here") " is a short installation guide. "
                        "Click " (h-link "NEWS.html" "here")
                        " for information about changes in the new version. ")
        ,(h-sub-section "ECB 1.96 released! (2003-09-15)"
                        "Support for window-managers like winring and escreen and therefore possibilty to run apps like Gnus and ECB in one frame. Complete overhaul of the compile-window mechanism - now it is much more stable. Some other nice features and bug fixes. "
                        (h-link "docs/Install-and-first-steps.html#Install%20and%20first%20steps" "Here") " is a short installation guide. "
                        "Click " (h-link "NEWS.html" "here")
                        " for information about changes in the new version. ")
        ,(h-sub-section "ECB 1.95.1 released! (2003-07-16)"
                        "Now every ECB-window can be maximized, so afterwards only this ecb-window and the edit-window(s) are visible. Some bug fixes. "
                        (h-link "docs/Install-and-first-steps.html#Install%20and%20first%20steps" "Here") " is a short installation guide. "
                        "Click " (h-link "NEWS.html" "here")
                        " for information about changes in the new version. ")
        ,(h-sub-section "ECB 1.95 released! (2003-07-07)"
                        "ECB-tree-windows now use image-icons. Hideshow was added to the popup-menu of the Methods-buffer. "
                        (h-link "docs/Install-and-first-steps.html#Install%20and%20first%20steps" "Here") " is a short installation guide. "
                        "Click " (h-link "NEWS.html" "here")
                        " for information about changes in the new version. ")
        ,(h-sub-section "ECB 1.94 now available as XEmacs package 1.08! (2003-06-30)"
                        "The XEmacs-package ECB 1.08 can be installed either via "
                        (h-link "http://www.xemacs.org/Download/win32/setup.exe"
                                "XEmacs-netinstaller")
                        " (for Windows) or from " (h-link "ftp://ftp.xemacs.org:/pub/xemacs/packages/" "ftp.xemacs.org")
                        " or via the package-manager of XEmacs.")
        ,(h-sub-section "ECB 1.94 released! (2003-06-23)"
                        "Additional native parsing/displaying of source-contents of imenu- or etags-supported source-types (e.g. perl, TeX, LaTeX...). "
                        (h-link "docs/Install-and-first-steps.html#Install%20and%20first%20steps" "Here") " is a short installation guide. "
                        "Click " (h-link "NEWS.html" "here")
                        " for information about changes in the new version. ")
        ,(h-sub-section "ECB 1.93 now available as XEmacs package 1.06! (2003-04-09)"
                        "The XEmacs-package ECB 1.06 can be installed either via "
                        (h-link "http://www.xemacs.org/Download/win32/setup.exe"
                                "XEmacs-netinstaller")
                        " (for Windows) or from " (h-link "ftp://ftp.xemacs.org:/pub/xemacs/packages/" "ftp.xemacs.org")
                        " or via the package-manager of XEmacs.")
        ,(h-sub-section "ECB 1.93 released! (2003-03-27)"
                        "Fixes some bugs and enhances the layout-engine. Offers also some new features. "
                        (h-link "docs/Install-and-first-steps.html#Install%20and%20first%20steps" "Here") " is a short installation guide. "
                        "Click " (h-link "NEWS.html" "here")
                        " for information about changes in the new version. ")
        ,(h-sub-section "ECB 1.92.1 released! (2003-03-05)"
                        "This is a bugfix-release without new features. "
                        (h-link "docs/Install-and-first-steps.html#Install%20and%20first%20steps" "Here") " is a short installation guide. "
                        "Click " (h-link "NEWS.html" "here")
                        " for information about changes in the new version. ")
        ,(h-sub-section "Bugfixes for ECB 1.92 available. (2003-02-26)"
                        "Click " (h-link "downloads.html" "here") " to get it.")
        ,(h-sub-section "ECB 1.92 released! (2003-02-24)"
                        "This release fixes some bugs and enhances layout and tree-buffer customizing. "
                        (h-link "docs/Install-and-first-steps.html#Install%20and%20first%20steps" "Here") " is a short installation guide. "
                        "Click " (h-link "NEWS.html" "here")
                        " for information about changes in the new version. ")
        ,(h-sub-section "ECB 1.91.1 released! (2003-02-11)"
                        "This is mostly a bugfix-release which fixes some annoying bugs! "
                        (h-link "docs/Install-and-first-steps.html#Install%20and%20first%20steps" "Here") " is a short installation guide. "
                        "Click " (h-link "NEWS.html" "here")
                        " for information about changes in the new version. ")
        ,(h-sub-section "Bugfixes for ECB 1.90 available. (2003-02-06)"
                        "Click " (h-link "downloads.html" "here") " to get it.")
        ,(h-sub-section "Usermanual now available ín PDF-format! (2003-02-03)"
                        "Click " (h-link "downloads.html" "here") " to get
                        it.")
        ,(h-sub-section "ECB 1.80 is now an official XEmacs package! (2003-02-01)"
                        "The ECB XEmacs-package has the version-number 1.01 and can "
                        "be installed either via "
                        (h-link "http://www.xemacs.org/Download/win32/setup.exe"
                                "XEmacs-netinstaller")
                        " (for Windows) or from " (h-link "ftp://ftp.xemacs.org:/pub/xemacs/packages/" "ftp.xemacs.org")
                        " or via the package-manager of XEmacs.")
        ,(h-sub-section "ECB 1.90 released! (2003-01-31)"
                        "A lot of new features! Fixed some annoying bugs! "
                        (h-link "docs/Install-and-first-steps.html#Install%20and%20first%20steps" "Here") " is a short installation guide. "
                        "Click " (h-link "NEWS.html" "here")
                        " for information about changes in the new version. ")
        ,(h-sub-section "ECB has a new official website! (2003-01-30)"
                        "You are already visiting the "
                        (h-link "main.html" "new webiste")
                        ". The "
                        (h-link "http://home.swipnet.se/mayhem/ecb.html"
                                "old website")
                        " is not longer supported!")
        ,(h-sub-section "ECB has a new maintainer. (2003-01-30)"
                        "Maintainance of ECB has been moved from "
                        (h-email "mayhem@home.se" "Jesper Nordenberg")
                        " to "
                        (h-email "klaus.berndl@sdm.de" "Klaus Berndl")
                        ".")
        ,(h-sub-section "ECB 1.80 released! (2002-08-12)")
        ,(h-sub-section "ECB 1.70 released! (2002-03-01)")
        ,(h-sub-section "ECB 1.60 released! (2002-01-20)"
                        "Many improvements. Works fine with Emacs 21.")
        ,(h-sub-section "ECB 1.52 released! (2001-10-24)"
                        "Fixed a small bug when loading ECB.")
        ,(h-sub-section "ECB 1.51 released! (2001-10-21)"
                        "Some new features.")
        ,(h-sub-section "ECB 1.50 released! (2001-08-12)"
                        "A couple of minor improvements and some bug fixes.")))


(defun ecb-html-main()
  (h-doc
   "main.html"
   "ECB - Emacs Code Browser"

   (h-section "About"
              (h-p "ECB stands for \"Emacs Code Browser\". While Emacs already has good "
                   (h-i "editing")
                   " support for many modes, its "
                   (h-i "browsing")
                   " support is somewhat"
                   " lacking. That's where ECB comes in: it displays a number of informational"
                   " windows that allow for easy source code navigation and overview.")

              (h-p "The informational windows can contain:")

              (h-p (h-bullet-list
                    "A directory tree,"
                    "a list of source files in the current directory (with full support and display of the VC-state),"
                    "a list of functions/classes/methods/... in the current file, (ECB uses the CEDET-semantic, or Imenu, or etags, for getting this list so all languages supported by any of these tools are automatically supported by ECB too)"
                    "a history of recently visited files (groupable by several criterias),"
                    "a direct and auto-updated ecb-window for the semantic-analyzer for some intellisense,"
                    "the Speedbar and"
                    (concat "output from compilation (the " (h-i "compilation") " window) and other modes like help, grep etc. or whatever a user defines to be displayed in this window.")
                    ))
   
              (h-p "As an added bonus, ECB makes sure to keep these informational windows visible,"
                   " even when you use C-x 1 and similar commands.")

              (h-p "It goes without saying that you can configure the layout, ie which"
                   " informational windows should be displayed where. ECB comes with a number of"
                   " ready-made window layouts to choose from."))
   
   (apply 'h-section "News"
          (append ecb-latest-news
                  `("Click "
                    ,(h-link "all-news.html" "here")
                    " to get a list of all news.")))

   (h-section "Dependencies"
	      (h-bullet-link-list
	       ecb-bullet
	       '(("http://cedet.sourceforge.net" "Full CEDET suite" "Recommend is the newest release, required is at least a version >= 1.0pre6.")
		 ("http://jdee.sourceforge.net/" "JDEE (optional)" "If you use ECB for Java development."))
		 "_top")
	      (h-p "If you use XEmacs you must have installed the packages mail-lib and c-support (contains hideshow.el)."))

   (h-section "Developers"
	      (h-bullet-link-list
	       ecb-bullet
	       '(
		 ("mailto:klaus.berndl@sdm.de" "Klaus Berndl")
		 ("mailto:zappo@gnu.org" "Eric M. Ludlam")
		 )))

   (h-section "Feedback"
	      "Please use " (h-link "http://lists.sourceforge.net/lists/listinfo/ecb-list" "the public ECB mailing list") " for reporting bugs, making suggestions and asking questions about ECB.")

   (h-table
    (h-tr (h-td "The page's WebCounter says that you are visitor number ")
	  (h-td	(h-img "http://counter.digits.com/wc/-d/4/javabrowser" "ALIGN=middle WIDTH=60 HEIGHT=20 BORDER=0 HSPACE=4 VSPACE=2"))
	  (h-td " since 2000-07-28.")))
   ))

(defun ecb-html-all-news()
  (h-doc
   "all-news.html"
   "ECB News"
   (apply 'h-section "All ECB news"
          (append ecb-latest-news
                  ecb-rest-news
                  `(,(h-line) ,(h-link "main.html" "Back") " to main-site.")))))

(defun ecb-html-old()
  (h-doc
   "javabrowser.html"
   "Page has moved"
   (h-p (h-b (h-link "index.html" "This page has moved.")))))

(defun ecb-html-logo()
  (h-doc
   "logo.html"
   "ECB Logo"
   (cons 'bgcolor h-section-title-bgcolor)
   '(topmargin . "2")
   '(leftmargin . "0")
   '(marginwidth . "0")
   '(marginheight . "0")
   (h-center
    (h-link "main.html" '(target . "main")
	    (h-img "ecb_logo.gif" "border='0'")))))
;;             (h-img "screenshot.png" "border='0'")))))

  
(defun ecb-html-links()
  (h-doc
   "links.html"
   "ECB Links"
   (h-section
    "Emacs Links"
    (h-bullet-link-list
     ecb-bullet
     (list
      (list "http://www.gnu.org/software/emacs/emacs.html" "GNU Emacs" (concat "No comment " (h-img "smiley.gif")))
       '("http://www.xemacs.org" "XEmacs" "")
       '("http://jdee.sourceforge.net/" "JDEE" "Recommended Java development environment for Emacs.")
       '("http://cedet.sourceforge.net" "CEDET" "A collection of Emacs development tools created by Eric M. Ludlam.")
       '("http://www.anc.ed.ac.uk/~stephen/emacs/ell.html" "Emacs Lisp List" "A good collection of Emacs lisp packages.")
	)
     "_top"))))

(defun ecb-html-menu()
  (let ((old h-section-text-bgcolor))
    (setq h-section-text-bgcolor ecb-menu-color)
    (h-doc
     "menu.html"
     "ECB Menu"
     '(leftmargin . "2")
     '(marginwidth . "2")
					;     '(marginheight . "2")
     (h-section
      "Sections"
      (h-bullet-link-list
       ecb-bullet
       '(
	 ("main.html" "Main")
         ("docs/Overview.html" "Overview")
         ("docs/Install-and-first-steps.html#Install%20and%20first%20steps" "Installation")
	 ("docs/index.html" "Documentation")
	 ("docs/FAQ.html#FAQ" "FAQ")
	 ("NEWS.html" "History")
	 ("downloads.html" "Downloads")
	 ("http://lists.sourceforge.net/lists/listinfo/ecb-list" "Mailing List")
	 ;; ("http://ecb.cvs.sourceforge.net/ecb/ecb/" "CVS")
	 ("cvs.html" "CVS")
	 ("screenshots/index.html" "Screenshots")
	 ("links.html" "Links")
	 )
       "main"))
;;      (h-p
;;      (h-b "Latest version: ") h-br
;;      (h-img ecb-bullet) " " (h-link ecb-zip-url (h-b ecb-zip-name)) h-br
;;      (h-img ecb-bullet) " " (h-link ecb-gz-url (h-b ecb-gz-name)))
     (h-p
      (h-b "Hosted by: ") h-br
      (h-link "http://sourceforge.net/projects/ecb" '(target . "_top") (h-img "http://sflogo.sourceforge.net/sflogo.php?group_id=17484&amp;type=13" "width='120' height='30' border='0' alt='Get ECB at SourceForge.net. Fast, secure and Free Open Source software downloads'"))) ;;
     (h-p
      (h-b "Updated: ") h-br
      (h-date))
;;      (h-p
;;       (h-b "Screenshot: ") h-br
;;       (h-link "screenshots/index.html" '(target . "_top") (h-img "screenshot.png" "width='140' height='113' border='0' alt='ECB Screenshot'")))
     )
  (setq h-section-text-bgcolor old)))

(defun ecb-html-downloads()
  (h-doc
   "downloads.html"
   "ECB Download Area"
   (h-section
    "ECB Download Area"
    (list
     h-br
     (h-sub-section
      "Download from SourceForge Download Area"
      "Go to the "
      (h-link "http://sourceforge.net/project/showfiles.php?group_id=17484"
              "ECB-Download Area") " at SourceForge.")
     (h-line)
     (h-sub-section
      "Download from CVS repository"
      (h-bullet-link-list
       ecb-bullet
       (list
        '("http://ecb.cvs.sourceforge.net/ecb/ecb/" "Full CVS repository" "Browse the CVS repository of ECB for downloading single files.")
        '("cvs_snapshots/ecb.tar.gz" "Latest CVS-shapshot" "Download the latest stable CVS-snapshot of ECB")
        )
       "_top"))
     (h-line)
     (h-sub-section
      "Download Patches"
      (concat "There are no patches available for current ECB " ecb-version "!"))
;;       (concat "Available patches for ECB " ecb-version ":")
;;       (h-bullet-link-list
;;        ecb-bullet
;;        (list
;;         ;;; Add here all patches which should offered directly on the website.
;;         '("patches/ecb.el" "ecb.el" "Fixes a bug which always truncates lines in the ECB-windows regardless of the setting in ecb-truncate-lines.")
;;         '("patches/ecb-navigate.el" "ecb-navigate.el" "Fixes a bug in ECB 1.90 which can inhibit that a user can open sources or clicking onto methods.")
;;         '("patches/ecb-layout.el" "ecb-layout.el" "Fixes a bug in the command ecb-store-window-sizes of ECB 1.90.")
;;         )
;;        "_top")
;;       h-br "Instructions:"
;;       (h-numbered-list
;;        "Download the patched files you need."
;;        "Replace the old-versions in the ECB-installation directory with the new downloaded versions."
;;        (concat "Re-byte-compile ECB with the command "
;;                (h-i "ecb-byte-compile")
;;                " if you use ECB byte-compiled.")
;;        "Restart Emacs and ECB.")
     (h-line)
     (h-sub-section
      "Download third party tools"
      (h-bullet-link-list
       ecb-bullet
       (list
        '("http://www.python.org/emacs/winring/" "winring.el" "A nifty window-manager written by Barry A. Warsaw")
        '("http://www.splode.com/~friedman/software/emacs-lisp/#ui" "escreen.el" "Another nifty window-manager written by Noah Friedman")
        )
       "_top"))
     (h-line)
     (h-link "main.html" "Back") " to main section")     
     )))

(defun ecb-html-cvs()
  (h-doc
   "cvs.html"
   "ECB CVS Area"
   (h-section
    "ECB CVS Area"
    (list
     h-br
     (h-sub-section
      "Browse the CVS repository"
      "Go to the "
      (h-link "http://ecb.cvs.sourceforge.net/ecb/ecb/"
              "ECB CVS browser") " at SourceForge.")
     (h-line)
     (h-sub-section
      "Anonymous checkout of the full ECB CVS repository"
      "Perform the following command:"
      h-br
      h-br
      (h-i "cvs -z3 -d:pserver:anonymous@ecb.cvs.sourceforge.net:/cvsroot/ecb co -P ecb"))
     (h-line)
     (h-link "main.html" "Back") " to main section")     
    )))

     
(defun ecb-html-top()
  (h-doc
   "top.html"
   "ECB Top"
   (cons 'bgcolor ecb-menu-color)
   '(topmargin . "0")
   '(margiwidth . "0")
   '(marginheight . "0")
   '(link . "#0000bb")
   '(vlink . "#004040")
   '(alink . "#00a000")
   (h-table
    (h-tr (h-td '(nowrap) '(width . "100%") (h-fsize "5" (h-b "Emacs Code Browser")))
	  (h-td '(nowrap) (h-email "klaus.berndl@sdm.de" (h-img "mail.gif" "border='0'"))))
    )))


;; -------------------- HTML generation --------------------------------------

(ecb-html-top)
(ecb-html-menu)
(ecb-html-main)
(ecb-html-logo)
(ecb-html-links)
(ecb-html-old)
(ecb-html-all-news)
(ecb-html-downloads)
(ecb-html-cvs)
