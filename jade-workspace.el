;;; jade-workspace.el --- Use local files for debugging          -*- lexical-binding: t; -*-

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

;; Setup a workspace for using local files when debugging JavaScript.
;;
;; Files are looked up using a special `.jade' file placed in the root directory
;; of the files served.

;;; Example:

;; With the following directory structure:
;;
;; www/
;;    index.html
;;    css/
;;       style.css
;;    js/
;;       app.js
;;    .jade
;;
;; For the following URL "http://localhost:3000/js/app.js"
;; `jade-workspace-lookup-file' will return "./www/js/app.js".

;;; Code:

(require 'url)
(require 'seq)
(require 'map)
(require 'subr-x)

(defun jade-workspace-lookup-file (url)
  "Return a local file matching URL in the current workspace directories.
If no file is found, return nil."
  (if-let ((root (jade-workspace-root)))
      (let* ((path (seq-drop (car (url-path-and-query
                                  (url-generic-parse-url url)))
                            1))
            (file (expand-file-name path root)))
        (when (file-exists-p file)
          file))))

(defun jade-workspace-make-url (file connection)
  "Return the url associated with the local FILE for CONNECTION.
The url is built using `jade-workspace-root'."
  (if-let ((root (jade-workspace-root)))
      (let* ((url (jade-workspace--url-basepath (map-elt connection 'url)))
             (path (file-relative-name file root)))
        (setf (url-filename url) (jade-workspace--absolute-path path))
        (url-recreate-url url))))

(defun jade-workspace--absolute-path (path)
  "Return PATH as absolute.
Prepend a \"/\" to PATH unless it already starts with one."
  (unless (string= (seq-take path 1) "/")
    (concat "/" path)))

(defun jade-workspace--url-basepath (url)
  "Return an urlobj with the basepath of URL.
The path and query string of URL are stripped."
  (let ((urlobj (url-generic-parse-url url)))
    (url-parse-make-urlobj (url-type urlobj)
                           (url-user urlobj)
                           (url-password urlobj)
                           (url-host urlobj)
                           (url-port urlobj)
                           nil nil nil t)))

(defun jade-workspace-root ()
  "Lookup the root workspace directory from the current buffer."
  (jade-workspace-locate-dominating-file default-directory ".jade"))

(defun jade-workspace-locate-dominating-file (file name)
  "Look up the directory hierarchy from FILE for a directory containing NAME.
Stop at the first parent directory containing a file NAME,
and return the directory.  Return nil if not found.
Instead of a string, NAME can also be a predicate taking one argument
\(a directory) and returning a non-nil value if that directory is the one for
which we're looking."
  ;; copied from projectile.el, itself copied from files.el (stripped comments)
  ;; emacs-24 bzr branch 2014-03-28 10:20
  (setq file (abbreviate-file-name file))
  (let ((root nil)
        try)
    (while (not (or root
                    (null file)
                    (string-match locate-dominating-stop-dir-regexp file)))
      (setq try (if (stringp name)
                    (projectile-file-exists-p (expand-file-name name file))
                  (funcall name file)))
      (cond (try (setq root file))
            ((equal file (setq file (file-name-directory
                                     (directory-file-name file))))
             (setq file nil))))
    (and root (expand-file-name (file-name-as-directory root)))))

(provide 'jade-workspace)
;;; jade-workspace.el ends here
