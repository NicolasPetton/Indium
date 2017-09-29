;;; indium-workspace.el --- Use local files for debugging          -*- lexical-binding: t; -*-

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
;; Files are looked up using a special `.indium' file placed in the root directory
;; of the files served.
;;

;;; Example:

;; With the following directory structure:
;;
;; project/ (current directory)
;;    www/
;;       index.html
;;       css/
;;          style.css
;;       js/
;;          app.js
;;       .indium
;;
;; For the following URL "http://localhost:3000/js/app.js"
;; `indium-workspace-lookup-file' will return "./www/js/app.js".
;;
;; Previously used workspace directories are saved and stored in
;; "~/.emacs.d/indium-workspaces.el".  You can change its location by
;; customizing `indium-workspace-file'.
;;
;; To disable persisting the list of workspaces to a file, you can set
;; `indium-workspace-use-workspace-file' to `nil'.  In this case,
;; `indium-workspaces' can be manually set in your emacs.d.


;;
;;; Code:

(require 'url)
(require 'seq)
(require 'map)
(require 'subr-x)

(require 'indium-structs)
(require 'indium-backend)

(declare-function indium-repl-get-buffer "indium-repl.el")

(defgroup indium-workspace nil
  "Indium workspace"
  :prefix "indium-worspace-"
  :group 'indium)

(defcustom indium-workspace-file (locate-user-emacs-file "indium-workspaces.el")
  "Location of the file used to store previously used workspace directories."
  :type 'file)

(defcustom indium-workspace-use-workspace-file t
  "Persist the list of worskpaces used in a file."
  :type 'boolean)

(defvar indium-workspaces nil
  "List of previously used workspace directories.")

(defun indium-workspace-read ()
  "Ask the user to select a workspace directory.
If the current directory is within a workspace, simply return it
without prompting the user.

The selected workspace directory is added to the list of workspaces."
  (when indium-workspace-use-workspace-file
    (indium-workspace--read-workspaces-file))
  (let ((workspace (or (indium-workspace-root)
                       (when (and indium-workspaces
                                  (y-or-n-p "No workspace found.  Select one? "))
                         (completing-read "Choose a workspace: " indium-workspaces)))))
    (when (and workspace indium-workspace-use-workspace-file)
      (indium-workspace--add-directory workspace)
      (indium-workspace--save-workspaces-file)
      workspace)))

(defun indium-workspace-lookup-file (url &optional ignore-existence)
  "Return a local file matching URL for the current connection.
If no file is found, and IGNORE-EXISTENCE is nil, return nil,
otherwise return the path of a file that does not exist."
  (when url
    (or (indium-workspace--lookup-using-file-protocol url)
        (indium-workspace--lookup-using-workspace url ignore-existence))))

(defun indium-workspace-lookup-file-safe (url)
  "Find a local file for URL, or return URL is no file can be found."
  (or (indium-workspace-lookup-file url) url))

(defun indium-workspace--lookup-using-file-protocol (url)
  "Return a local file matching URL if URL use the file:// protocol."
  (when (indium-workspace--file-protocol-p)
    (let* ((url (url-generic-parse-url url))
           (path (car (url-path-and-query url))))
      (when (file-regular-p path)
        path))))

(defun indium-workspace--lookup-using-workspace (url &optional ignore-existence)
  "Return a local file matching URL using the current Indium workspace.
When IGNORE-EXISTENCE is non-nil, also match file paths that are
not on disk."
  ;; Make sure we are in the correct directory so that indium can find a
  ;; ".indium" file.
  ;;
  ;; TODO: set the directory in the connection directly instead of relying on
  ;; the REPL buffer
  (with-current-buffer (indium-repl-get-buffer)
    (if-let ((root (indium-workspace-root)))
	(let* ((path (seq-drop (car (url-path-and-query
				     (url-generic-parse-url url)))
			       1))
	       (file (expand-file-name path root)))
	  (when (or ignore-existence (file-regular-p file))
	    file)))))

(defun indium-workspace-make-url (file)
  "Return the url associated with the local FILE."
  (or (indium-workspace--make-url-using-file-path file)
      (indium-workspace--make-url-using-file-protocol file)
      (indium-workspace--make-url-using-workspace file)))

(defun indium-workspace--make-url-using-file-path (file)
  "When using nodejs, the path of FILE should be used directly."
  (when (indium-connection-nodejs-p indium-current-connection)
    file))

(defun indium-workspace--make-url-using-file-protocol (file)
  "Return a url using the \"file://\" protocol for FILE.
If the current connection doesn't use the file protocol, return nil."
  (when (indium-workspace--file-protocol-p)
    (format "file://%s" file)))

(defun indium-workspace--make-url-using-workspace (file)
  "Return the url associated with the local FILE.
The url is built using `indium-workspace-root'."
  (if-let ((root (indium-workspace-root)))
      (let* ((url (indium-workspace--url-basepath (indium-current-connection-url)))
             (path (file-relative-name file root)))
        (setf (url-filename url) (indium-workspace--absolute-path path))
        (url-recreate-url url))))

(defun indium-workspace--file-protocol-p ()
  "Return non-nil if the current connection use the file protocol."
  (let ((url (url-generic-parse-url (indium-current-connection-url))))
    (string= (url-type url) "file")))

(defun indium-workspace--absolute-path (path)
  "Return PATH as absolute.
Prepend a \"/\" to PATH unless it already starts with one."
  (unless (string= (seq-take path 1) "/")
    (concat "/" path)))

(defun indium-workspace--url-basepath (url)
  "Return an urlobj with the basepath of URL.
The path and query string of URL are stripped."
  (let ((urlobj (url-generic-parse-url url)))
    (url-parse-make-urlobj (url-type urlobj)
                           (url-user urlobj)
                           (url-password urlobj)
                           (url-host urlobj)
                           (url-port urlobj)
                           nil nil nil t)))

(defun indium-workspace-root ()
  "Lookup the root workspace directory from the current buffer."
  (indium-workspace-locate-dominating-file default-directory ".indium"))

(defun indium-workspace-locate-dominating-file (file name)
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
                    (file-exists-p (expand-file-name name file))
                  (funcall name file)))
      (cond (try (setq root file))
            ((equal file (setq file (file-name-directory
                                     (directory-file-name file))))
             (setq file nil))))
    (and root (expand-file-name (file-name-as-directory root)))))

(defun indium-workspace--add-directory (directory)
  "Add DIRECTORY to the list of workspaces."
  (add-to-list 'indium-workspaces directory))

(defun indium-workspace--save-workspaces-file ()
  "Save previously used workspace directories."
  (make-directory (file-name-directory indium-workspace-file) t)
  (with-temp-file indium-workspace-file
    (emacs-lisp-mode)
    (insert ";; This file is automatically generated by Indium.")
    (newline)
    (insert (format "(setq %s '%S)" "indium-workspaces" indium-workspaces))))

(defun indium-workspace--read-workspaces-file ()
  "Read the workspaces file and set `indium-worspaces'."
  (load indium-workspace-file t))

(provide 'indium-workspace)
;;; indium-workspace.el ends here
