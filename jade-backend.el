;;; jade-backend.el --- Backend for jade.el          -*- lexical-binding: t; -*-

;; Copyright (C) 2016  Nicolas Petton

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
(require 'jade-repl)

(defvar jade-connections (list) "List of connections.")

(defvar jade-connection nil
  "Current connection to the browser tab.

A connection should be an alist with the following required keys:
`backend' and `url'.  Other backend-specific keys might be used
by backends.")
(make-variable-buffer-local 'jade-connection)

(defvar jade-backends nil "List of registered backends.")

(defmacro jade-with-connection (connection &rest body)
  "Set the value of `jade-connection' to CONNECTION and evaluate BODY."
  (declare (debug t) (indent 1))
  `(let ((jade-connection ,connection))
     ,@body))

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
    (jade-backend-close-connection (jade-backend) jade-connection)
    (setq jade-connections (remq jade-connection jade-connections))
    (jade-backend-kill-all-buffers jade-connection)))

(defun jade-reconnect ()
  "Try to re-establish a connection.
The new connection is based on the current (usually closed) one."
  (interactive)
  (unless jade-connection
    (user-error "No connection associated to the current buffer"))
  (jade-backend-reconnect (jade-backend)))

(defun jade-backend-kill-all-buffers (connection)
  "Kill all buffers that have the `jade-connection' CONNECTION."
  (seq-map #'kill-buffer
           (seq-filter (lambda (buf)
                         (with-current-buffer buf
                           (eq jade-connection connection)))
                       (buffer-list))))

(defun jade-reload ()
  "Reload the page."
  (interactive)
  (jade-backend-evaluate (jade-backend) "window.location.reload()"))

(defun jade-active-connections ()
  "Return a list of all active connections."
  (seq-filter (lambda (connection)
                (jade-backend-active-connection-p (map-elt connection 'backend) connection))
              jade-connections))

;;; jade-connection methods

(cl-defgeneric jade-backend-active-connection-p (backend connection)
  "Return non-nil if CONNECTION is active."
  t)

(cl-defgeneric jade-backend-close-connection (backend connection)
  "Close CONNECTION.")

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

(cl-defgeneric jade-backend-evaluate-on-frame (backend string frame &optional callback)
  "Evaluate STRING on the call frame FRAME then call CALLBACK.
CALLBACK is called with two arguments, the value returned by the
evaluation and non-nil if the evaluation threw an error.

The value should be an alist with a the following required keys:
`type', `value' and `description'.  If the value represents a
remote object that can be inspected, it should also have an
`objectid' key.")

(cl-defgeneric jade-backend-get-completions (backend expression prefix callback)
  "Get the completion list using CONNECTION for EXPRESSION that match PREFIX.
Evaluate CALLBACK on the filtered candidates.

EXPRESSION should be a valid JavaScript expression string.")

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
