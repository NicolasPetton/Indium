;;; indium-backend.el --- Backend for indium.el          -*- lexical-binding: t; -*-

;; Copyright (C) 2016-2018  Nicolas Petton

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

;; Backends should define a new backend symbol using `indium-register-backend'.
;; Once a connection to a JavaScript runtime is established by the backend, it
;; should set `indium-current-connection'.

;;; Code:

(require 'map)
(require 'seq)
(require 'indium-debugger-litable)
(eval-and-compile (require 'indium-structs))

(declare 'indium-debugger-unset-current-buffer)

(defgroup indium-backend nil
  "Indium backend."
  :prefix "indium-backend-"
  :group 'indium)

(defcustom indium-connection-open-hook nil
  "Hook called after a connection is open."
  :group 'indium-backend
  :type 'hook)

(defcustom indium-connection-closed-hook nil
  "Hook called after a connection is closed."
  :group 'indium-backend
  :type 'hook)

(defvar indium-backends nil "List of registered backends.")

(defvar indium-script-parsed-hook nil "Hook run when a new script is parsed.")

(defun indium-register-backend (backend)
  "Register a new BACKEND.
BACKEND should be a symbol."
  (add-to-list 'indium-backends backend))

(declare-function indium-repl-get-buffer "indium-repl.el")
(declare-function indium-debugger-unset-current-buffer "indium-debugger.el")

(defun indium-backend-cleanup-buffers ()
  "Cleanup all Indium buffers."
  (seq-map (lambda (buf)
             (with-current-buffer buf
               (when buffer-file-name
                 (indium-debugger-unset-current-buffer))))
           (buffer-list))
  (when-let ((buf (indium-repl-get-buffer)))
    (kill-buffer buf)))

(cl-defgeneric indium-backend-active-connection-p (_backend)
  "Return non-nil if the current connection is active."
  t)

(cl-defgeneric indium-backend-close-connection (_backend)
  "Close the current connection.

Concrete implementations should run `indium-connection-closed-hook'.")

(cl-defgeneric indium-backend-reconnect (_backend)
  "Try to re-establish a connection.
The new connection is created based on the current
`indium-current-connection'.")

(cl-defgeneric indium-backend-evaluate (backend string &optional callback)
  "Evaluate STRING then call CALLBACK.
CALLBACK is called with two arguments, the value returned by the
evaluation and non-nil if the evaluation threw an error.

The value should be an alist with a the following required keys:
`type', `value' and `description'.  If the value represents a
remote object that can be inspected, it should also have an
`objectid' key.")

(cl-defgeneric indium-backend-get-completions (backend expression prefix callback)
  "Get the completion for EXPRESSION that match PREFIX.
Evaluate CALLBACK on the filtered candidates.

EXPRESSION should be a valid JavaScript expression string.")

(cl-defgeneric indium-backend-register-breakpoint (backend breakpoint &optional callback)
  "Request the addition of BREAKPOINT.")

(cl-defgeneric indium-backend-unregister-breakpoint (backend id &optional callback)
  "Request the removal of the breakpoint with id ID.")

(cl-defgeneric indium-backend-deactivate-breakpoints (backend)
  "Deactivate all breakpoints.
The runtime will not pause on any breakpoint."
  )

(cl-defgeneric indium-backend-activate-breakpoints (backend)
  "Deactivate all breakpoints.
The runtime will not pause on any breakpoint."
  )

(cl-defgeneric indium-backend-set-script-source (backend url source &optional callback)
  "Update the contents of the script at URL to SOURCE.")

(cl-defgeneric indium-backend-get-properties (backend reference &optional callback all-properties)
  "Request the properties of the remote object represented by REFERENCE.
REFERENCE must be the id of a remote object.
CALLBACK is called with the fetched list of properties.

If ALL-PROPERTIES is non-nil, get all the properties from the
prototype chain of the remote object.")

(cl-defgeneric indium-backend-get-script-source (backend frame callback)
  "Get the source of the script for FRAME.
Evaluate CALLBACK with the result.")

(cl-defgeneric indium-backend-resume (backend &optional callback)
  "Resume the debugger and evaluate CALLBACK if non-nil.")

(cl-defgeneric indium-backend-step-into (backend &optional callback)
  "Step into the current stack frame and evaluate CALLBACK if non-nil.")

(cl-defgeneric indium-backend-step-out (backend &optional callback)
  "Step out the current stack frame and evaluate CALLBACK if non-nil.")

(cl-defgeneric indium-backend-step-over (backend &optional callback)
  "Step over the current stack frame and evaluate CALLBACK if non-nil.")

(cl-defgeneric indium-backend-continue-to-location (backend location &optional callback)
  "Continue to LOCATION and evaluate CALLBACK if non-nil.")

(defun indium-backend-object-reference-p (value)
  "Return non-nil if VALUE is a reference to a remote object."
  (map-elt value 'objectid))

(provide 'indium-backend)
;;; indium-backend.el ends here
