;;; jade-workspace.el --- Use local files fog debugging          -*- lexical-binding: t; -*-

;; Copyright (C) 2017  Nicolas Petton

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

;;

;;; Code:

(require 'url)
(require 'seq)
(require 'subr-x)

(defvar jade-workspace-directories nil
  "List of directories containing JavaScript files.")

(defun jade-workspace-lookup-file (url)
  "Return a local file matching URL in the current workspace directories."
  (let ((path (seq-drop (car (url-path-and-query (url-generic-parse-url url))) 1)))
    (seq-some (lambda (directory)
                (let ((file (expand-file-name path directory)))
                  (when (file-exists-p file)
                    file)))
              jade-workspace-directories)))

(provide 'jade-workspace)
;;; jade-workspace.el ends here
