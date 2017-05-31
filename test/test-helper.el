;;; test-helper.el --- Helpers for running Indium tests  -*- lexical-binding: t; -*-

;; Copyright (C) 2016-2017  Nicolas Petton

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

(when (require 'undercover nil t)
  (setq undercover-force-coverage t)
  (undercover "*.el" (:exclude "run-lint.el" "test/*.el")))

(advice-add 'undercover-report :after #'print-coverage-report-safe)

(defun print-coverage-report-safe (&rest _)
  (ignore-errors
    (print-coverage-report)))

(defun print-coverage-report ()
  (let* ((coverage (apply #'concatenate 'list
                          (seq-map (lambda (src)
                                     (let ((coverage (map-elt src 'coverage)))
                                       (seq-filter #'identity coverage)))
                                   (map-elt (json-read-file "/tmp/undercover_coveralls_report") 'source_files))))
         (covered-lines (seq-filter (lambda (line)
                                      (not (zerop line)))
                                    coverage))
         (percentage (round (* (/ (seq-length covered-lines) (seq-length coverage) 1.0) 100))))
    (message "%d%% covered" percentage)))


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
  "Evaluate BODY with CONNECTION as the indium-connection."
  (declare (indent 1))
  `(let ((indium-connection ,connection))
     ,@body))

(defmacro with-fake-indium-connection (&rest body)
  "Evaluate BODY with an indium connection with a fake backend."
  (declare (indent 0))
  `(with-indium-connection '((backend . fake))
     ,@body))

(defmacro with-nodejs-connection (&rest body)
  "Run BODY within a NodeJS connection on a process on fixtures/test.js."
  (declare (indent 0))
  `(progn
     (indium-run-node "node fixtures/test.js")
     (sleep-for 1)
     ,@body
     (kill-nodejs-process)))

(defun kill-nodejs-process ()
  "Kill the nodejs process if any."
  (when-let ((buf (get-buffer "*node process*")))
    (when-let ((process (get-buffer-process buf)))
      (set-process-query-on-exit-flag process nil))
    (kill-buffer buf))
  (ignore-errors
    (indium-quit)))

(defmacro with-repl-buffer (&rest body)
  "Execute BODY within a REPL buffer with a NodeJS connection."
  (declare (indent 0))
  `(with-nodejs-connection
     (with-current-buffer (indium-repl-buffer-name)
       ,@body)))

(defun repl-eval (expression)
  "Send EXPRESSION to be evaluated.
Onece EXPRESSION has been sent for evaluation, sleep for 500ms to
give time for the runtime to send a response."
  (insert expression)
  (press-and-sleep-for "RET" 0.5))

(defun press (keybinding)
  "Call interactively the command bound to KEYBINDING."
  (call-interactively (key-binding (kbd keybinding))))

(defun press-and-sleep-for (keybinding seconds)
  "Call `(press KEYBINDING)` and sleep for SECONDS."
  (press keybinding)
  (sleep-for seconds))

(provide 'test-helper)
;;; test-helper.el ends here
