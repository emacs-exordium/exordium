;;; eieio-test-mro.el --- eieio method resolution order tests
;;
;; Copyright (C) 2009, 2010 Jan Moringen
;;
;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>
;; Keywords: oop, lisp, tools
;;
;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.
;;
;; Please send bug reports, etc. to zappo@gnu.org


;;; Commentary:
;;
;; Tests for the various method resolution orders.


;;; History:
;;


;;; Code:
;;

(require 'eieio)


;;; Grid Case
;;

(dolist (fixture '((:breadth-first
		    . (confused-grid
		       hv-grid vh-grid
		       horizontal-grid vertical-grid
		       grid-layout
		       eieio-default-superclass))
		   (:depth-first
		    . (confused-grid
		       hv-grid horizontal-grid
                       grid-layout eieio-default-superclass
		       vertical-grid vh-grid))
		   (:c3 . error)))
  (let ((mro      (first fixture))
	(expected (rest fixture)))
    (eval
     `(progn
	(defclass grid-layout ()
	  ()
	  ""
	  :method-invocation-order ,mro)

	(defclass horizontal-grid (grid-layout)
	  ()
	  ""
	  :method-invocation-order ,mro)

	(defclass vertical-grid (grid-layout)
	  ()
	  ""
	  :method-invocation-order ,mro)

	(defclass hv-grid (horizontal-grid vertical-grid)
	  ()
	  ""
	  :method-invocation-order ,mro)

	(defclass vh-grid (vertical-grid horizontal-grid)
	  ()
	  ""
	  :method-invocation-order ,mro)

	(defclass confused-grid (hv-grid vh-grid)
	  ()
	  ""
	  :method-invocation-order ,mro)))

    (let ((result
	   (condition-case err
	       (class-precedence-list 'confused-grid)
	     (inconsistent-class-hierarchy 'error))))
      (assert (equal result expected)))))


;;; Boat Case
;;

(dolist (fixture '((:breadth-first
		    . (pedalo
		       pedal-wheel-boat small-catamaran
		       engine-less wheel-boat small-multihull
		       day-boat boat eieio-default-superclass))
		   (:depth-first
		    . (pedalo
		       pedal-wheel-boat engine-less day-boat boat
		       eieio-default-superclass
		       wheel-boat small-catamaran small-multihull))
		   (:c3
		    . (pedalo
		       pedal-wheel-boat engine-less
		       small-catamaran small-multihull
		       day-boat wheel-boat boat
		       eieio-default-superclass))))
  (let ((mro      (first fixture))
	(expected (rest fixture)))
    (eval
     `(progn
	(defclass boat ()
	  ()
	  ""
	  :method-invocation-order ,mro)

	(defclass day-boat (boat)
	  ()
	  ""
	  :method-invocation-order ,mro)

	(defclass wheel-boat (boat)
	  ()
	  ""
	  :method-invocation-order ,mro)

	(defclass engine-less (day-boat)
	  ()
	  ""
	  :method-invocation-order ,mro)

	(defclass small-multihull (day-boat)
	  ()
	  ""
	  :method-invocation-order ,mro)

	(defclass pedal-wheel-boat (engine-less wheel-boat)
	  ()
	  ""
	  :method-invocation-order ,mro)

	(defclass small-catamaran (small-multihull)
	  ()
	  ""
	  :method-invocation-order ,mro)

	(defclass pedalo (pedal-wheel-boat small-catamaran)
	  ()
	  ""
	  :method-invocation-order ,mro)))

    (let ((result
	   (condition-case err
	       (class-precedence-list 'pedalo)
	     (inconsistent-class-hierarchy 'error))))
      (assert (equal result expected)))))


;;; Widget Case
;;

(dolist (fixture '((:breadth-first
		    (popup-menu
		     menu popup-mixin choice-widget
		     eieio-default-superclass)
		    (new-popup-menu
		     menu popup-mixin choice-widget
		     eieio-default-superclass))
		   (:depth-first
		    (popup-menu
		     menu choice-widget eieio-default-superclass
		     popup-mixin)
		    (new-popup-menu
		     menu choice-widget eieio-default-superclass
		     popup-mixin))
		   (:c3
		    (popup-menu
		     menu choice-widget popup-mixin
		     eieio-default-superclass)
		    (new-popup-menu
		     menu popup-mixin choice-widget
		     eieio-default-superclass))))
  (let ((mro        (first  fixture))
	(expected-1 (second fixture))
	(expected-2 (third  fixture)))
    (eval
     `(progn
	(defclass choice-widget ()
	  ()
	  ""
	  :method-invocation-order ,mro)

	(defclass popup-mixin ()
	  ()
	  ""
	  :method-invocation-order ,mro)

	(defclass menu (choice-widget)
	  ()
	  ""
	  :method-invocation-order ,mro)

	(defclass popup-menu (menu popup-mixin)
	  ()
	  ""
	  :method-invocation-order ,mro)

	(defclass new-popup-menu (menu popup-mixin choice-widget)
	  ()
	  ""
	  :method-invocation-order ,mro)))

    (let ((result
	   (condition-case err
	       (class-precedence-list 'popup-menu)
	     (inconsistent-class-hierarchy 'error))))
      (assert (equal result expected-1)))

    (let ((result
	   (condition-case err
	       (class-precedence-list 'new-popup-menu)
	     (inconsistent-class-hierarchy 'error))))
      (assert (equal result expected-2))))
  )


;;; Another Widget Case
;;

(dolist (fixture '((:breadth-first
		    . (editable-scrollable-pane
		       scrollable-pane editable-pane
		       pane scrolling-mixin editing-mixin
		       eieio-default-superclass))
		   (:depth-first
		    . (editable-scrollable-pane
		       scrollable-pane pane eieio-default-superclass
		       scrolling-mixin editable-pane editing-mixin))
		   (:c3
		    . (editable-scrollable-pane
		       scrollable-pane editable-pane
		       pane scrolling-mixin editing-mixin
		       eieio-default-superclass))))
  (let ((mro      (first  fixture))
	(expected (rest fixture)))
    (eval
     `(progn
	(defclass pane ()
	  ()
	  ""
	  :method-invocation-order ,mro)

	(defclass scrolling-mixin ()
	  ()
	  ""
	  :method-invocation-order ,mro)

	(defclass editing-mixin ()
	  ()
	  ""
	  :method-invocation-order ,mro)

	(defclass scrollable-pane (pane scrolling-mixin)
	  ()
	  ""
	  :method-invocation-order ,mro)

	(defclass editable-pane (pane editing-mixin)
	  ()
	  ""
	  :method-invocation-order ,mro)

	(defclass editable-scrollable-pane (scrollable-pane editable-pane)
	  ()
	  ""
	  :method-invocation-order ,mro)))

    (let ((result
	   (condition-case err
	       (class-precedence-list 'editable-scrollable-pane)
	     (inconsistent-class-hierarchy 'error))))
      (assert (equal result expected)))))

(provide 'eieio-test-mro)
;;; eieio-test-mro.el ends here
