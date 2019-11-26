;;; indium-structs.el --- CL structs for Indium backends  -*- lexical-binding: t; -*-

;; Copyright (C) 2017-2018  Nicolas Petton

;; Author: Nicolas Petton <nicolas@petton.fr>
;; Keywords:

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; This files defines all objects used in Indium as cl-structs.
;;
;; Backends should make instances of the structs defined in this file from data
;; they receive.
;;
;; `indium-location' represents a location (most often to a file).  A location
;; is a struct with a `line' and `column' slot.  If a location points to a local
;; file, it also contains a `file' slot.  Columns are 0-based and lines are
;; 1-based.
;;
;; `indium-frame' represents a call frame in the context of debugging.
;;
;; `indium-breakpoint' represents a breakpoint set at a location with a possible
;; breaking condition.

;;; Code:

(require 'map)
(require 'subr-x)

(cl-defstruct
    (indium-location (:constructor indium-location-create)
		     (:constructor indium-location-at-point
				   (&aux (file buffer-file-name)
					 (line (line-number-at-pos))
					 (column (current-column))))
		     (:constructor indium-location-from-alist
				   (alist &aux
					  (file (map-elt alist 'file))
					  (line (map-elt alist 'line))
					  (column (map-elt alist 'column))))
		     (:copier nil))
  (line 1)
  (column 0)
  (file nil))

(cl-defstruct (indium-breakpoint
	       (:constructor indium-breakpoint-create
			     (&key condition
				   overlay
				   id))
	       (:copier nil))
  (id (indium-structs--next-breakpoint-id))
  (overlay nil)
  (resolved nil)
  (condition ""))

(defun indium-breakpoint-location (brk)
  "Return the location of BRK."
  (when-let ((ov (indium-breakpoint-overlay brk))
	     (pos (overlay-start ov))
	     (buf (overlay-buffer ov)))
    (with-current-buffer buf
      (save-excursion
	(goto-char (point-min))
	(forward-char (1- pos))
	(back-to-indentation)
	(indium-location-at-point)))))

(defun indium-breakpoint-buffer (breakpoint)
  "Return the buffer in which BREAKPOINT is set, or nil."
  (when-let ((ov (indium-breakpoint-overlay breakpoint)))
    (overlay-buffer ov)))

(defun indium-breakpoint-unresolved-p (breakpoint)
  "Return non-nil if BREAKPOINT is not yet resolved in the runtime."
  (not (indium-breakpoint-resolved breakpoint)))

(cl-defstruct (indium-frame
	       (:constructor indium-frame-create
			     (&key script-id
				   function-name
				   location
				   scope-chain))
	       (:constructor indium-frame-from-alist
			     (alist &aux
                                    (id (map-elt alist 'id))
				    (script-id (map-elt alist 'scriptId))
				    (function-name (map-elt alist 'functionName))
				    (location (indium-location-from-alist
					       (map-elt alist 'location)))
				    (scope-chain (seq-map #'indium-scope-from-alist
							  (map-elt alist 'scopeChain)))))
	       (:copier nil))
  (id "")
  (function-name "")
  (script-id "")
  (location nil)
  (scope-chain nil))

(cl-defstruct (indium-scope
	       (:constructor indium-scope-create
			     (&key type
				   name
				   id))
	       (:constructor indium-scope-from-alist
			     (alist &aux
				    (type (map-elt alist 'type))
				    (name (map-elt alist 'name))
				    (id (map-elt alist 'id))))
	       (:copier nil))
  (id "")
  (name "")
  (type ""))

(cl-defstruct (indium-remote-object
	       (:constructor indium-remote-object-create
			     (&key id
				   type
				   description
				   preview))
	       (:constructor indium-remote-object-from-alist
			     (alist &aux
				    (id (map-elt alist 'id))
				    (type (map-elt alist 'type))
				    (description (map-elt alist 'description))
				    (preview (map-elt alist 'preview))))
	       (:copier nil))
  (id nil)
  (type "")
  (description "")
  (preview ""))

(defun indium-remote-object-error-p (obj)
  "Return non-nil if OBJ represents an error."
  (equal (indium-remote-object-type obj) "error"))

(defun indium-remote-object-reference-p (obj)
  "Return non-nil if OBJ is a reference to a remote object."
  (let ((id (indium-remote-object-id obj)))
    (and (not (null id))
	 (not (string-empty-p id)))))

(defun indium-remote-object-function-p (obj)
  "Return non-nil if OBJ represents a function."
  (equal (indium-remote-object-type obj) "function"))

(defun indium-remote-object-has-preview-p (obj)
  "Return non-nil if OBJ has a preview string."
  (let ((preview (indium-remote-object-preview obj)))
    (and preview (not (string-empty-p preview)))))

(defun indium-remote-object-to-string (obj &optional full)
  "Return a short string describing OBJ.

When FULL is non-nil, do not strip long descriptions and function
definitions."
  (if (and (not full) (indium-remote-object-function-p obj))
      "function"
    (indium-remote-object-description obj)))

(cl-defstruct (indium-property
	       (:constructor indium-property-create
			     (&key name
				   remote-object))
	       (:constructor indium-property-from-alist
			     (alist &aux
				    (name (map-elt alist 'name))
				    (remote-object (indium-remote-object-from-alist
						    (map-elt alist 'value)))))
	       (:copier nil))
  (name "")
  (remote-object nil))

(defun indium-property-native-p (property)
  "Return non-nil value if PROPERTY is native code."
  (string-match-p "{ \\[native code\\] }$"
                  (or (indium-remote-object-description
		       (indium-property-remote-object
			property))
		      "")))

(defvar indium-structs--breakpoint-id 0
  "A number that gets incremented by `indium-structs--next-breakpoint-id'.")

(defun indium-structs--next-breakpoint-id ()
  "Return a number, different at each call."
  (cl-incf indium-structs--breakpoint-id)
  indium-structs--breakpoint-id)

(provide 'indium-structs)
;;; indium-structs.el ends here
