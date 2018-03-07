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
;; `indium-script' represents a JavaScript file parsed by the runtime.  Scripts
;; are structs indexed by `id' in the current Indium connection.  A script contain
;; an `url' slot, and an optional `sourcemap-url' slot.
;;
;; `indium-location' represents a location (most often to a file).  A location
;; is a struct with a `line' and `column' slot.  If a location points to a local
;; file, it also contains a `file' slot.  Columns and lines start at 0.
;;
;; `indium-frame' represents a call frame in the context of debugging.
;;
;; `indium-breakpoint' represents a breakpoint set at a location with a possible
;; breaking condition.

;;; Code:

(require 'map)
(require 'subr-x)

(declare-function indium-script-get-file "indium-script.el")
(declare-function indium-script-find-by-id "indium-script.el")

(defmacro when-indium-connected (&rest body)
  "Evaluate BODY if there is a current Indium connection."
  (declare (indent 0) (debug t))
  `(when indium-current-connection
     ,@body))

(defmacro unless-indium-connected (&rest body)
  "Evalute BODY unless there is a current Indium connection."
  (declare (indent 0))
  `(unless indium-current-connection
     ,@body))

(defvar indium-current-connection nil
  "Current connection to the browser tab.")

(cl-defstruct (indium-connection)
  (backend nil :type symbol :read-only t)
  (url nil :type string :read-only t)
  ;; Optional process attached to the connection (used by NodeJS)
  (process nil :type process)
  (callbacks (make-hash-table) :type hash-table)
  (scripts (make-hash-table) :type hash-table)
  (frames nil :type list)
  (current-frame nil :type indium-frame)
  ;; extra properties that can be added by the backend
  (props (make-hash-table) :type hash-table))

(defun indium-current-connection-backend ()
  "Return the backend of the current connection if any."
  (when-indium-connected
    (indium-connection-backend indium-current-connection)))

(defun indium-current-connection-url ()
  "Return the url of the current connection if any."
  (when-indium-connected
   (indium-connection-url indium-current-connection)))

(defun indium-current-connection-callbacks ()
  "Return the callbacks of the current connection if any."
  (when-indium-connected
    (indium-connection-callbacks indium-current-connection)))

(defun indium-current-connection-process ()
  "Return the process attached to the current connection if any."
  (when-indium-connected
    (indium-connection-process indium-current-connection)))

(cl-defmethod (setf indium-current-connection-process) (process)
  (when-indium-connected
    (setf (indium-connection-process indium-current-connection) process)))

(cl-defmethod (setf indium-current-connection-callbacks) (callbacks)
  (when-indium-connected
    (setf (indium-connection-callbacks indium-current-connection) callbacks)))

(defun indium-current-connection-scripts ()
  "Return the scripts of the current connection if any."
  (when-indium-connected
    (indium-connection-scripts indium-current-connection)))

(defun indium-current-connection-props ()
  "Return the props of the current connection if any."
  (when-indium-connected
    (indium-connection-props indium-current-connection)))

(defun indium-current-connection-frames ()
  "Return the frames of the current connection if any."
  (when-indium-connected
    (indium-connection-frames indium-current-connection)))

(cl-defmethod (setf indium-current-connection-frames) (frames)
  (when-indium-connected
    (setf (indium-connection-frames indium-current-connection) frames)))

(defun indium-current-connection-current-frame ()
  "Return the current frame of the current connection if any."
  (when-indium-connected
    (indium-connection-current-frame indium-current-connection)))

(cl-defmethod (setf indium-current-connection-current-frame) (frame)
  (when-indium-connected
    (setf (indium-connection-current-frame indium-current-connection) frame)))

(cl-defstruct indium-script
  (id nil :type string :read-only t)
  (url nil :type string :read-only t)
  (sourcemap-url nil :type string :read-only t)
  (parsed-time (current-time) :read-only t)
  ;; Keep a cache of the parsed sourcemap for speed.  See
  ;; `indium-script-sourcemap'.
  (sourcemap-cache nil))

(cl-defstruct
    (indium-location
     (:constructor make-indium-location-from-script-id
		   (&key (script-id "")
			 line
			 column
			 &aux (file (indium-script-get-file (indium-script-find-by-id script-id))))))
  (line 0 :type number :read-only t)
  (column 0 :type number :read-only t)
  (file nil :type string :read-only t))

(cl-defstruct indium-frame
  (id nil :type string :read-only t)
  ;; TODO: make a scope a struct as well.
  (scope-chain nil :type list :read-only t)
  (location nil :type indium-location :read-only t)
  (script nil :type indium-script :read-only t)
  (type nil :type string :read-only t)
  (function-name nil :type string))

(cl-defstruct (indium-breakpoint
	       (:constructor nil)
	       (:constructor make-indium-breakpoint
			     (&key id
				   line
				   column
				   file
				   condition
				   overlay
				   &aux (location (make-indium-location
						   :line line
						   :column column
						   :file file)))))
  (id nil :type string)
  (overlay nil)
  (local-file buffer-file-name :type string)
  (location nil :type indium-location :read-only t)
  (condition "" :type string))

(defun indium-breakpoint-buffer (breakpoint)
  "Return the buffer in which BREAKPOINT is set, or nil."
  (when-let ((ov (indium-breakpoint-overlay breakpoint)))
    (overlay-buffer ov)))

(defun indium-breakpoint-unresolved-p (breakpoint)
  "Return non-nil if BREAKPOINT is not yet resolved in the runtime."
  (null (indium-breakpoint-id breakpoint)))

(defun indium-breakpoint-unresolve (breakpoint)
  "Remove the resolution information from BREAKPOINT."
  (setf (indium-breakpoint-id breakpoint) nil))

(provide 'indium-structs)
;;; indium-structs.el ends here
