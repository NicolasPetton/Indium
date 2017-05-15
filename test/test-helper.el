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
  (undercover "*.el"))

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
         (percentage (* (/ (seq-length covered-lines) (seq-length coverage) 1.0) 100)))
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

(provide 'test-helper)
;;; test-helper.el ends here
