;;; jade-backend.el --- Backend for jade.el          -*- lexical-binding: t; -*-

;; Copyright (C) 2016-2017  Nicolas Petton

;; Author: Nicolas Petton <nicolas@petton.fr>
;; Keywords: internal

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

;; Generic backend implementation.

;; Backends should define a new backend symbol using `jade-register-backend',

;;; Code:

(require 'map)
(require 'seq)
(require 'jade-repl)
(require 'jade-debugger-litable)

(declare 'jade-debugger-unset-current-buffer)

(defvar jade-connection nil
  "Current connection to the browser tab.

A connection should be an alist with the following required keys:
`backend' and `url'.  Other backend-specific keys might be used
by backends.")

(defvar jade-backends nil "List of registered backends.")

(defun jade-backend ()
  "Return the backend for the current connection."
  (map-elt jade-connection 'backend))

(defun jade-register-backend (backend)
  "Register a new BACKEND.
BACKEND should be a symbol."
  (add-to-list 'jade-backends backend))

(defun jade-quit ()
  "Close the current connection and kill its REPL buffer if any.
When called interactively, prompt for a confirmation first."
  (interactive)
  (unless jade-connection
    (user-error "No active connection to close"))
  (when (or (not (called-interactively-p 'interactive))
            (y-or-n-p (format "Do you really want to close the connection to %s ? "
                              (map-elt jade-connection 'url))))
    (jade-backend-close-connection (jade-backend))
    (jade-backend-cleanup-buffers)
    (setq jade-connection nil)))

(defun jade-reconnect ()
  "Try to re-establish a connection.
The new connection is based on the current (usually closed) one."
  (interactive)
  (unless jade-connection
    (user-error "No Jade connection to reconnect to"))
  (jade-backend-reconnect (jade-backend)))

(defun jade-backend-cleanup-buffers ()
  "Cleanup all Jade buffers."
  (seq-map (lambda (buf)
             (with-current-buffer buf
               (when buffer-file-name
                 (jade-debugger-unset-current-buffer))))
           (buffer-list))
  (when-let ((buf (jade-repl-get-buffer)))
    (kill-buffer buf)))

;;; jade-connection methods

(cl-defgeneric jade-backend-active-connection-p (backend)
  "Return non-nil if the current connection is active."
  t)

(cl-defgeneric jade-backend-close-connection (backend)
  "Close the current connection.")

(cl-defgeneric jade-backend-reconnect (backend)
  "Try to re-establish a connection.
The new connection is created based on the current
`jade-connection'.")

(cl-defgeneric jade-backend-evaluate (backend string &optional callback)
  "Evaluate STRING then call CALLBACK.
CALLBACK is called with two arguments, the value returned by the
evaluation and non-nil if the evaluation threw an error.

The value should be an alist with a the following required keys:
`type', `value' and `description'.  If the value represents a
remote object that can be inspected, it should also have an
`objectid' key.")

(cl-defgeneric jade-backend-get-completions (backend expression prefix callback)
  "Get the completion for EXPRESSION that match PREFIX.
Evaluate CALLBACK on the filtered candidates.

EXPRESSION should be a valid JavaScript expression string.")

(cl-defgeneric jade-backend-add-breakpoint (backend file line &optional callback condition)
  "Request the addition of a breakpoint.

The breakpoint is addet to FILE on line LINE.  When CALLBACK is
non-nil, evaluate it with the breakpoint's location and id.")

(cl-defgeneric jade-backend-remove-breakpoint (backend id)
  "Request the removal of the breakpoint with id ID.")

(cl-defgeneric jade-backend-get-breakpoints (backend)
  "Return all breakpoints.
A breakpoint is a map with the keys `id', `file', and `line'.")

(defun jade-backend-get-breakpoints-in-file (file)
  "Return all breakpoints in FILE."
  (let ((breakpoints (jade-backend-get-breakpoints (jade-backend))))
    (seq-filter (lambda (brk)
                  (string= (map-elt brk 'file) file))
                breakpoints)))

(cl-defgeneric jade-backend-get-properties (backend reference &optional callback all-properties)
  "Request the properties of the remote object represented by REFERENCE.
REFERENCE must be the id of a remote object.
CALLBACK is called with the fetched list of properties.

If ALL-PROPERTIES is non-nil, get all the properties from the
prototype chain of the remote object.")

(cl-defgeneric jade-backend-get-script-source (backend frame callback)
  "Get the source of the script for FRAME.
Evaluate CALLBACK with the result.")

(cl-defgeneric jade-backend-get-script-url (backend frame)
  "Return the url of the script for FRAME, or nil.")

(cl-defgeneric jade-backend-resume (backend &optional callback)
  "Resume the debugger and evaluate CALLBACK if non-nil.")

(cl-defgeneric jade-backend-step-into (backend &optional callback)
  "Step into the current stack frame and evaluate CALLBACK if non-nil.")

(cl-defgeneric jade-backend-step-out (backend &optional callback)
  "Step out the current stack frame and evaluate CALLBACK if non-nil.")

(cl-defgeneric jade-backend-step-over (backend &optional callback)
  "Step over the current stack frame and evaluate CALLBACK if non-nil.")

(cl-defgeneric jade-backend-continue-to-location (backend location &optional callback)
  "Continue to LOCATION and evaluate CALLBACK if non-nil.
Location should be an alist with a `column' and `row' key.")

(defun jade-backend-object-reference-p (value)
  "Return non-nil if VALUE is a reference to a remote object."
  (map-elt value 'objectid))

(provide 'jade-backend)
;;; jade-backend.el ends here
