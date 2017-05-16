;;; run-lint.el --- Lint project files                         -*- lexical-binding: t; -*-

;; Copyright (C) 2017  Nicolas Petton

;; Author: Nicolas Petton <nicolas@petton.fr>

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

;; Run checkdoc in batch mode on all project files.

;;; Code:

(require 'checkdoc)
(require 'seq)

(let ((files (seq-filter (lambda (file)
                           (string= (file-name-extension file) "el"))
                         (directory-files "."))))
  (seq-doseq (file files)
    (with-current-buffer (find-file file)
      (message "Linting %s..." file)
      (checkdoc-current-buffer))))

(provide 'run-lint)
;;; run-lint.el ends here
