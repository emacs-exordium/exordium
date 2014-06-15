;;; ede/arduino.el --- EDE support for arduino projects / sketches
;;
;; Copyright (C) 2012, 2013 Eric M. Ludlam
;;
;; Author: Eric M. Ludlam <eric@siege-engine.com>
;;
;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation, either version 3 of the
;; License, or (at your option) any later version.

;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see http://www.gnu.org/licenses/.

;;; Commentary:
;;
;; EDE support for arduino projects.
;;
;; For most basic configurations, you will still need to use the
;; arduino IDE to set and change preferences.  Use the command
;; 'ede-arduino-sync' to syncrhonize with changes made in the arduino
;; IDE.
;;
;;
;; To download an arduino mode for your code, see this mode:
;; https://github.com/bookest/arduino-mode

(require 'ede)

(declare-function data-debug-show-stuff "data-debug")

;;; Code:
(defcustom ede-arduino-makefile-name "Makefile"
  "File name to use for generated Makefile."
  :group 'ede
  :type 'file)

(defcustom ede-arduino-preferences-file "~/.arduino/preferences.txt"
  "The location of personl preferences for the arduino IDE.
Note: If this changes, we need to also update the autoload feature."
  :group 'arduino
  :type 'string)

;;; CLASSES
;;
;; The classes for arduino projects include arduino (PDE) files, plus C, CPP, and H files.
;;
(defclass ede-arduino-target (ede-target)
  ()
  "EDE Arduino C files target.  Includes PDE, C, C++ and anything else we find.")

(defclass ede-arduino-project (ede-project)
  ((keybindings :initform (("U" . ede-arduino-upload)))
   (menu :initform
	 (
	  [ "Upload Project to Board" ede-arduino-upload ]
	  [ "Serial Monitor" cedet-arduino-serial-monitor ]
	  "--"
	  [ "Edit Projectfile" ede-edit-file-target
	    (ede-buffer-belongs-to-project-p) ]
	  "--"
	  [ "Update Version" ede-update-version ede-object ]
	  [ "Version Control Status" ede-vc-project-directory ede-object ]
	  "--"
	  [ "Rescan Project Files" ede-rescan-toplevel t ]
	  ))
   )
  "EDE Arduino project.")

;;; TARGET MANAGEMENT
;;
(defmethod ede-find-target ((proj ede-arduino-project) buffer)
  "Find an EDE target in PROJ for BUFFER.
If one doesn't exist, create a new one for this directory."
  (let* ((targets (oref proj targets))
	 (dir default-directory)
	 (ans (object-assoc dir :path targets))
	 )
    (when (not ans)
      (setq ans (ede-arduino-target dir
                 :name (file-name-nondirectory
			(directory-file-name dir))
		 :path dir
		 :source nil))
      (object-add-to-list proj :targets ans)
      )
    ans))

;;;###autoload
(defun ede-arduino-root (&optional dir basefile)
  "Get the root project directory for DIR.
The only arduino sketches allowed are those configured by the arduino IDE
in their sketch directory.

If BASEFILE is non-nil, then convert root to the project basename also.

Consider expanding this at some later date."
  (when (ede-arduino-find-install) ;; Do nothing if tools aren't installed.
    (let* ((prefs (ede-arduino-sync))
	   (sketchroot (and prefs (oref prefs sketchbook)))
	   )
      (when (and
	     sketchroot
	     (< (length sketchroot) (length dir))
	     (string= sketchroot (substring dir 0 (length sketchroot))))
	;; The subdir in DIR just below sketchroot is always the root of this
	;; project.
	(let* ((dirtail (substring dir (length sketchroot)))
	       (dirsplit (split-string dirtail "/" t))
	       (root (expand-file-name (car dirsplit) sketchroot)))
	  (when (file-directory-p root)
	    (if basefile
		(let ((tmp (expand-file-name (concat (car dirsplit) ".pde") root)))
		  ;; Also check for the desired file in a buffer if the
		  ;; user just made the file but not saved it yet.
		  (when (or (not (file-exists-p tmp)) (not (get-file-buffer tmp)))
		    (setq tmp (expand-file-name (concat (car dirsplit) ".ino") root)))
		  tmp)
	      root)))))))

;;;###autoload
(defun ede-arduino-file (&optional dir)
  "Get a file representing the root of this arduino project.
It is a file ending in .pde or .ino that has the same basename as
the directory it is in.  Optional argument DIR is the directory
to check."
  (ede-arduino-root (or dir (expand-file-name default-directory)) t))

;;;###autoload
(defun ede-arduino-load (dir &optional rootproj)
  "Return an Arduino project object if there is one.
Return nil if there isn't one.
Argument DIR is the directory it is created for.
ROOTPROJ is nil, sinc there is only one project for a directory tree."
  (let* ((root (ede-arduino-root dir))
	 (proj (and root (ede-directory-get-open-project root))))
    (if proj
	proj

      (when root
	;; Create a new project here.
	(let* ((name (file-name-nondirectory (directory-file-name root)))
	       (pde (expand-file-name (concat name ".pde") root)))
	  (when (not (file-exists-p pde))
	    (setq pde (expand-file-name (concat name ".ino") root)))
	  (setq proj (ede-arduino-project
		      name
		      :name name
		      :directory (file-name-as-directory dir)
		      :file pde
		      :targets nil)))
	(ede-add-project-to-global-list proj)
	))))

;;;###autoload
(add-to-list 'ede-project-class-files
	     (ede-project-autoload "arduino"
	      :name "ARDUINO SKETCH"
	      :file 'ede/arduino
	      :proj-root-dirmatch
	      (ede-project-autoload-dirmatch 
	       "arduino"
	       :fromconfig "~/.arduino/preferences.txt"
	       :configregex "^sketchbook.path=\\([^\n]+\\)$"
	       :configregexidx 1)
	      :proj-file 'ede-arduino-file
	      :proj-root 'ede-arduino-root
	      :load-type 'ede-arduino-load
	      :class-sym 'ede-arduino-project
	      :safe-p t
	      :new-p t)
	     t)

;;; COMMAND SUPPORT
;;
(defun ede-arduino-upload ()
  "Compile the current project, and upload the result to the board."
  (interactive)
  (project-compile-project (ede-current-project) "make all upload"))

;; Autoloaded through `serial-term'
(declare-function term-line-mode "term")

(defun cedet-arduino-serial-monitor ()
  "Start up a serial monitor for a running arduino board.
Uses `serial-term'."
  (interactive)
  (let ((prefs (ede-arduino-sync)))
    ;; @TODO - read the setup function for something configuring the
    ;; serial line w/ a baud rate, and use that.
    (serial-term (oref prefs port) 9600)
    ;; Always go to line mode, as arduino serial isn't typically used
    ;; for input, just debugging output.
    (term-line-mode)
    ))

(defmethod project-compile-project ((proj ede-arduino-project) &optional command)
  "Compile the entire current project PROJ.
Argument COMMAND is the command to use when compiling."
  ;; 1) Create the mini-makefile.
  (ede-arduino-create-makefile proj)
  ;; 2) Call MAKE
  (compile (or command "make"))
  )

(defmethod project-compile-target ((obj ede-arduino-target) &optional command)
  "Compile the current target OBJ.
Argument COMMAND is the command to use for compiling the target."
  (project-compile-project (ede-current-project) command))

(defmethod project-debug-target ((target ede-arduino-target))
  "Run the current project derived from TARGET in a debugger."
  (error "No Debugger support for Arduino."))

;;; C/C++ support
(require 'semantic/db)
(defmethod ede-preprocessor-map ((this ede-arduino-target))
  "Get the pre-processor map for some generic C code."
  ;; wiring.h and pins_arduino.h have lots of #defines in them.
  (let* ((wiring_h (expand-file-name "hardware/arduino/cores/arduino/wiring.h"
				     (ede-arduino-find-install)))
	 (table (when (and wiring_h (file-exists-p wiring_h))
		  (semanticdb-file-table-object wiring_h)))
	 (filemap '( ("HIGH" . "0x1")
		     ("LOW" . "0x0")
		     ))
	 )
    (when table
      (when (semanticdb-needs-refresh-p table)
	(semanticdb-refresh-table table))
      (setq filemap (append filemap (oref table lexical-table)))
      )
    filemap
    ))

(defmethod ede-system-include-path ((this ede-arduino-target))
  "Get the system include path used by project THIS."
  (let* ((prefs (ede-arduino-sync))
	 (iphardware (expand-file-name "hardware/arduino/cores/arduino"
				       (ede-arduino-find-install)))
	 (libs (ede-arduino-guess-libs))
	 (iplibs (mapcar
		  (lambda (lib)
		    (expand-file-name (concat "libraries/" lib)
				      (ede-arduino-find-install)))
		  libs)))
    (cons iphardware iplibs)))

;;; Makefile Creation
;;
;; Use SRecode, and the ede/srecode tool to build our Makefile.
(require 'ede/srecode)

(defun ede-arduino-regenerate ()
  "Force recreation of makefiles for arduino project."
  (interactive)
  (ede-arduino-create-makefile (ede-current-project)))

(defmethod ede-arduino-create-makefile ((proj ede-arduino-project))
  "Create an arduino based Makefile for project PROJ."
  (let* ((mfilename (expand-file-name ede-arduino-makefile-name
				     (oref proj directory)))
	 (prefs (ede-arduino-sync))
	 (board (oref prefs boardobj))
	 (vers (ede-arduino-Arduino-Version))
	 (sketch (ede-arduino-guess-sketch))
	 (orig-buffer nil)
	 (buff-to-kill nil))
    (when (and (string= (file-name-extension sketch) "ino")
	       (version< vers "1.0"))
      (error "Makefile doesn't support .ino files until Arduino 1.0"))
    (when (and (string= (file-name-extension sketch) "pde")
	       (version<= "1.0" vers))
      (error "Makefile doesn't support .pde files after Arduino 1.0"))

    (save-current-buffer
      (setq orig-buffer (get-file-buffer mfilename))
      (set-buffer (setq buff-to-kill (find-file-noselect mfilename)))
      (save-excursion
	(goto-char (point-min))
	(if (and
	     (not (eobp))
	     (not (looking-at "# Automatically Generated \\w+ by EDE.")))
	    (if (not (y-or-n-p (format "Really replace %s? " mfilename)))
		(error "Not replacing Makefile"))
	  (message "Replaced EDE Makefile"))
	(erase-buffer)
	(ede-srecode-setup)
	;; Insert a giant pile of stuff that is common between
	;; one of our Makefiles, and a Makefile.in
	(ede-srecode-insert
	 "arduino:ede-empty"
	 "TARGET" (oref proj name)
	 "ARDUINO_LIBS" (mapconcat 'identity (ede-arduino-guess-libs) " ")
	 "MCU" (oref board mcu)
	 "F_CPU" (oref board f_cpu)
	 "PORT" (oref prefs port)
	 "BOARD" (oref prefs board)
	 "AVRDUDE_ARD_BAUDRATE" (oref board speed)
	 "AVRDUDE_ARD_PROGRAMMER" (oref board protocol)
	 "ARDUINO_MK" (ede-arduino-Arduino.mk)
	 "ARDUINO_HOME" (ede-arduino-find-install)
	 ))
      (save-buffer)
      (when (not orig-buffer) (kill-buffer (current-buffer)))
      )))

;;; Arduino Sketch Code Inspector
;;
;; Inspect the code in an arduino sketch, and guess things, like which libraries to include.

(defun ede-arduino-guess-libs ()
  "Guess which libraries this sketch use."
  (interactive)
  (let* ((libs nil)
	 (libdir nil)
	 (sketch (ede-arduino-guess-sketch))
	 (orig-buffer (get-file-buffer sketch))
	 (buff nil)
	 (tmp nil))
    (save-current-buffer
      (setq buff (find-file-noselect sketch))
      (set-buffer buff)
      (save-excursion
	(goto-char (point-min))
	(while (re-search-forward "#include <\\(\\w+\\).h>" nil t)
	  (setq tmp (match-string 1))
	  (unless (file-exists-p (concat tmp ".h"))
	    (let* ((lib (match-string 1))
		   (libdir (ede-arduino-libdir lib))
		   (util (expand-file-name "utility" libdir)))
	      ;; Some libraries need a utility added to the library list.
	      (when (file-exists-p util)
		(push (concat lib "/utility") libs))
	      ;; Push real lib after the utility
	      (push lib libs)
	      )))))
    (when (not orig-buffer) (kill-buffer buff))
    libs))
    

(defun ede-arduino-guess-sketch ()
  "Return the file that is the core of the current project sketch."
  (let* ((proj ede-object-project)
	 (sketch (expand-file-name (concat (oref proj name) ".pde")
				   (oref proj directory)))
	 )
    (if (file-exists-p sketch)
	sketch
      (setq sketch (expand-file-name (concat (oref proj name) ".ino")
				     (oref proj directory)))
      (if (file-exists-p sketch)
	  sketch
	(error "Cannot guess primary sketch file for project %s"
	       (eieio-object-name proj))))))

;;; Arduino Preferences
;;
;; Derive data from the arduino IDE's preferences.
;;
(defclass ede-arduino-prefs ()
  ((timestamp :initform nil)
   (prefssize :initform nil)
   (board :initform "uno")
   (port :initform "/dev/ttyUSB1")
   (sketchbook :initform "~/arduino")

   (boardobj :initform nil)
   )
  "Class containing arduino preferences.")

(defvar ede-arduino-active-prefs nil
  "The currently active preferences for Arduino development.")

(defun ede-arduino-sync ()
  "Synchronize arduino development preferences with the arduino IDE.
Synchronization pulls preferences from `ede-arduino-preferences-file'
for use in Emacs.  It does not copy preferences or changes made in
Emacs back to the Arduino IDE."
  (interactive)
  (when (not (file-exists-p ede-arduino-preferences-file))
    (if (y-or-n-p "Can't find arduino preferences.  Start IDE to configure? ")
	(ede-arduino)
      (error "EDE cannot build/upload arduino projects without preferences from the arduino IDE")))
  (ede-arduino-read-prefs ede-arduino-preferences-file)
  (when (called-interactively-p 'any)
      (require 'data-debug)
      (data-debug-show-stuff ede-arduino-active-prefs "Arduino Prefs"))
  ede-arduino-active-prefs)

(defun ede-arduino-read-prefs (prefsfile)
  "Read in arduino preferences from the PREFSFILE."
  (let* ((buff (get-file-buffer prefsfile))
	 (stats (file-attributes prefsfile))
	 (size (nth 7 stats))
	 (mod (nth 5 stats))
	 (board nil))

    (when (not ede-arduino-active-prefs)
      (setq ede-arduino-active-prefs (ede-arduino-prefs "Arduino Preferences")))

    ;; Only update the prefs if the prefs file changed.
    (when (or (not (oref ede-arduino-active-prefs timestamp))
	      (/= (or (oref ede-arduino-active-prefs prefssize) 0) size)
	      (not (equal (oref ede-arduino-active-prefs timestamp) mod)))

      (setq buff (get-buffer-create "*arduino prefs*"))
      (with-current-buffer buff
	(erase-buffer)
	(insert-file-contents prefsfile)

	(goto-char (point-min))
	(when (not (re-search-forward "^serial.port=" nil t))
	  (error "Cannot find serial.port from the arduino preferences"))
	(oset ede-arduino-active-prefs port
	      (buffer-substring-no-properties (point) (point-at-eol)))

	(goto-char (point-min))
	(when (not (re-search-forward "^board=" nil t))
	  (error "Cannot find board from the arduino preferences"))
	(setq board (buffer-substring-no-properties (point) (point-at-eol)))
	(oset ede-arduino-active-prefs board board)

	(goto-char (point-min))
	(when (not (re-search-forward "^sketchbook.path=" nil t))
	  (error "Cannot find sketchbook.path from the arduino preferences"))
	(oset ede-arduino-active-prefs sketchbook
	      (file-name-as-directory
	       (expand-file-name
		(buffer-substring-no-properties (point) (point-at-eol)))))
	)

      (kill-buffer buff)

      (oset ede-arduino-active-prefs boardobj (ede-arduino-board-data board))

      (oset ede-arduino-active-prefs prefssize size)
      (oset ede-arduino-active-prefs timestamp mod)
	
      )))

;;; Arduino Intuition
;;
;; Examine the environment to find arduino library locations
;; so we can call the utilities.

(defcustom ede-arduino-arduino-command "arduino"
  "The command used for starting the arduino IDE.
The IDE is actually a script, so the purpose here is only to look up
where the arduino APPDIR is.

If you are customizing this variable, consider the short-cut of just
customizing the `ede-arduino-appdir' variable instead."
  :group 'arduino
  :type 'string)

(defcustom ede-arduino-appdir nil
  "The location of the arduino build environment's application.
This is also where Arduino.mk will be found."
  :group 'arduino
  :type 'directory)

(defun ede-arduino ()
  "Launch the arduino IDE."
  (interactive)
  (let ((b (get-buffer-create "*Arduino IDE*"))
	(cd default-directory)
	)
    (with-current-buffer b
      (setq default-directory cd)
      (erase-buffer))
    (apply 'start-process "arduino" b ede-arduino-arduino-command nil)
    ))

(defun ede-arduino-find-install ()
  "Return the directory where arduino IDE code is installed."
  (if (and ede-arduino-appdir (file-exists-p ede-arduino-appdir))
      ede-arduino-appdir

    ;; Derive by looking up the arduino script.
    (let ((arduinofile ede-arduino-arduino-command))
      (when (and arduinofile
		 (not (file-exists-p arduinofile)))
	;; Look up where it might be...
	(setq arduinofile (locate-file arduinofile exec-path))

	(when (and arduinofile
		   (not (file-exists-p arduinofile)))
	  (error "Cannot find arduino command location"))

	(if (not arduinofile)
	    nil
	  (let ((buff (get-buffer-create "*arduino scratch*")))
	    (with-current-buffer buff
	      (erase-buffer)
	      (insert-file-contents arduinofile)
	      
	      (goto-char (point-min))

	      (when (not (re-search-forward "APPDIR=" nil t))
		(error "Cannot find APPDIR from the arduino command"))

	      (prog1
		  (setq ede-arduino-appdir
			(buffer-substring-no-properties (point) (point-at-eol)))
		(kill-buffer buff)))))))))

(defun ede-arduino-Arduino.mk ()
  "Return the location of Arduino's makefile helper."
  (expand-file-name "Arduino.mk" (ede-arduino-find-install)))

(defun ede-arduino-Arduino-Version ()
  "Return the version of the installed Arduino."
  (let ((vfile (expand-file-name "lib/version.txt" (ede-arduino-find-install))))
    (let ((buff (get-buffer-create "*arduino scratch*")))
      (with-current-buffer buff
	(erase-buffer)
	(insert-file-contents vfile)
	(goto-char (point-min))
	(prog1
	    (if (looking-at "[0-1]+:\\([.0-9]+\\)\\+")
		(match-string 1)
	      (buffer-substring-no-properties (point) (point-at-eol)))
	  (kill-buffer buff)
	  )))))
	  
(defun ede-arduino-boards.txt ()
  "Return the location of Arduino's boards.txt file."
  (file-expand-wildcards
   (expand-file-name "hardware/*/boards.txt" (ede-arduino-find-install))))

(defun ede-arduino-libdir (&optional library)
  "Return the full file location of LIBRARY.
If LIBRARY is not provided as an argument, just return the library directory."
  (let ((libdir (expand-file-name "libraries" (ede-arduino-find-install))))
    (if library
	(expand-file-name library libdir)
      libdir)))

;;; Arduino Board Reading
;; 
;; Load data from boards.txt
(defclass ede-arduino-board ()
  ((name :initarg :name
	 :initform nil
	 :documentation
	 "The name of the arduino board represented by this object.")
   (protocol :initarg :protocol
	     :initform nil
	     :documentation
	     "The protocol used to talk to the board.")
   (speed :initarg :speed
	  :initform nil
	  :documentation
	  "The SPEED of the arduino board's serial upload.")
   (maximum-size :initarg :maximum-size
	  :initform nil
	  :documentation
	  "The MAXIMUM_SIZE of the arduino board's uploadable target .")
   (mcu :initarg :mcu
	:initform nil
	:documentation
	"The MCU of the arduino board.")
   (f_cpu :initarg :f_cpu
	  :initform nil
	  :documentation
	  "The F_CPU of the arduino board.")
   (core :initarg :core
	 :initform nil
	 :documentation
	 "The core name for this board.")
   )
  "Class for containing key aspect of the arduino board.")

(defun ede-arduino-board-data (boardname)
  "Read in the data from baords.txt for BOARDNAME.
Data returned is the intputs needed for the Makefile."
  (let ((buff (get-buffer-create "*arduino boards*"))
	(name nil)
	(protocol nil)
	(speed nil)
	(size nil)
	(mcu nil)
	(f_cpu nil)
	(core nil)
	(boardfiles (ede-arduino-boards.txt))
	)

    (with-current-buffer buff
      (erase-buffer)
      (while boardfiles
	(insert-file-contents (car boardfiles))
	(setq boardfiles (cdr boardfiles)))

      (goto-char (point-min))
      (when (not (re-search-forward (concat "^" boardname ".name=") nil t))
	(error "Cannot find %s.name looking up board" boardname))
      (setq name (buffer-substring-no-properties (point) (point-at-eol)))

      (goto-char (point-min))
      (when (not (re-search-forward (concat "^" boardname ".upload.protocol=") nil t))
	(error "Cannot find %s.upload.protocol looking up board" boardname))
      (setq protocol (buffer-substring-no-properties (point) (point-at-eol)))

      (goto-char (point-min))
      (when (not (re-search-forward (concat "^" boardname ".upload.speed=") nil t))
	(error "Cannot find %s.upload.speed looking up board" boardname))
      (setq speed (buffer-substring-no-properties (point) (point-at-eol)))

      (goto-char (point-min))
      (when (not (re-search-forward (concat "^" boardname ".upload.maximum_size=") nil t))
	(error "Cannot find %s.upload.maximum_size looking up board" boardname))
      (setq size (buffer-substring-no-properties (point) (point-at-eol)))

      (goto-char (point-min))
      (when (not (re-search-forward (concat "^" boardname ".build.mcu=") nil t))
	(error "Cannot find %s.build.mcu looking up board" boardname))
      (setq mcu (buffer-substring-no-properties (point) (point-at-eol)))

      (goto-char (point-min))
      (when (not (re-search-forward (concat "^" boardname ".build.f_cpu=") nil t))
	(error "Cannot find %s.build.f_cpu looking up board" boardname))
      (setq f_cpu (buffer-substring-no-properties (point) (point-at-eol)))

      (goto-char (point-min))
      (when (not (re-search-forward (concat "^" boardname ".build.core=") nil t))
	(error "Cannot find %s.build.core looking up board" boardname))
      (setq core (buffer-substring-no-properties (point) (point-at-eol)))
      )

    (kill-buffer buff)

    (ede-arduino-board boardname
		       :name name
		       :protocol protocol
		       :speed speed
		       :maximum-size size
		       :mcu mcu
		       :f_cpu f_cpu
		       :core core)
    ))

(provide 'ede/arduino)

;; Local variables:
;; generated-autoload-file: "loaddefs.el"
;; generated-autoload-load-name: "ede/arduino"
;; End:

;;; arduino.el ends here
