;;; test-helper.el --- Helpers for running Indium tests  -*- lexical-binding: t; -*-

;; Copyright (C) 2016-2018  Nicolas Petton

;; Author: Nicolas Petton <nicolas@petton.fr>
;; Keywords: convenience

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

;; This package provides helpers for running Indium tests.

;;; Code:

(require 'seq)
(require 'map)
(require 'assess)

(defvar indium-nodejs--test-fs
  '((".indium.json" "{\"configurations\": [{\"type\": \"node\", \"command\": \"node\"}]}")
    ("index.js" "console.log('foo')")))

(defmacro with-js2-buffer (contents &rest body)
  "Evaluate BODY.

BODY is evaluated with the current buffer set to a JavaScript
buffer in `js2-mode' with CONTENTS."
  (declare (indent 1))
  `(with-temp-buffer
     (insert ,contents)
     (goto-char (point-max))
     (js2-mode)
     (js2-parse)
     (indium-interaction-mode 1)
     ,@body))

(defmacro with-temp-workspace-file (&rest body)
  "Evaluate BODY.
During the evaluation of BODY, `indium-workspace-file' is set to
a temporary file, which is removed afterwards."
  (declare (indent 0))
  `(let ((indium-workspace-file (make-temp-file "indium-workspace.el")))
     (prog1
         (progn ,@body)
       (ignore-errors (delete-file indium-workspace-file nil)))))

(defmacro with-indium-connection (connection &rest body)
  "Evaluate BODY with CONNECTION as the indium-current-connection."
  (declare (indent 1))
  `(let ((indium-current-connection ,connection))
     ,@body))

(defmacro with-fake-indium-connection (&rest body)
  "Evaluate BODY with an indium connection with a fake backend."
  (declare (indent 0))
  `(with-indium-connection (indium-connection-create :backend 'fake)
     ,@body))

(defmacro with-indium-test-fs (&rest body)
  "Run BODY with filesystem set to `indium-nodejs--test-fs'."
  (declare (indent 0) (debug t))
  `(assess-with-filesystem indium-nodejs--test-fs
     ,@body))

(defmacro with-js2-file (&rest body)
  "Evaluate BODY.
The current buffer is set to the \"index.js\" file from
`indium-nodejs--test-fs' during the evaluation of BODY."
  (declare (indent 0))
  `(with-indium-test-fs
     (with-current-buffer (find-file-noselect "index.js")
       (goto-char (point-min))
       (js2-mode)
       (js2-parse)
       (indium-interaction-mode 1)
       ,@body)))

(defmacro with-indium-nodejs-connection (&rest body)
  "Run BODY within a NodeJS connection."
  (declare (indent 0))
  `(progn
     (ignore-errors (exec-path-from-shell-initialize))
     (with-indium-test-fs
       (indium-launch)
       (wait-for-repl-buffer)
       ,@body
       (indium-quit))))

(defun wait-for-repl-buffer (&optional retry)
  "Wait 200ms for the REPL buffer to be ready.
If RETRY is the number of retries, is non-nil."
  (unless retry (setq retry 10))
  (sleep-for 0.2)
  (unless (or (get-buffer (indium-repl-buffer-name))
              (= retry 0))
    (wait-for-repl-buffer (1- retry)))
  (sleep-for 0.2))

(defmacro with-repl-buffer (&rest body)
  "Execute BODY within a REPL buffer with a NodeJS connection."
  (declare (indent 0))
  `(with-indium-nodejs-connection
     (with-current-buffer (indium-repl-buffer-name)
       ,@body)))

(defun repl-eval (expression)
  "Send EXPRESSION to be evaluated.
Onece EXPRESSION has been sent for evaluation, sleep for 500ms to
give time for the runtime to send a response."
  (insert expression)
  (press-and-sleep-for "RET" 2))

(defun press (keybinding)
  "Call interactively the command bound to KEYBINDING."
  (call-interactively (key-binding (kbd keybinding))))

(defun press-and-sleep-for (keybinding seconds)
  "Call `(press KEYBINDING)` and sleep for SECONDS."
  (press keybinding)
  (sleep-for seconds))

;;; test-helper.el ends here
