;;; indium-workspace.el --- Indium workspace management          -*- lexical-binding: t; -*-

;; Copyright (C) 2017-2018  Nicolas Petton

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

;; Indium workspace management.
;;
;; When connecting to a backend, Indium will lookup and read a project
;; configuration file `.indium.json' in the project root directory.

;;; .indium.json configuration file example:
;;
;; {
;;   "configurations": [
;; 	{
;; 	  "name": "Chrome",
;; 	  "type": "chrome",
;; 	  "url": "http://localhost:3333/"
;; 	},
;; 	{
;; 	  "name": "Node server",
;; 	  "type": "node",
;; 	  "command": "node ./src/server.js",
;; 	  "inspect-brk": false
;; 	}
;; 	{
;; 	  "name": "Gulp",
;; 	  "type": "node",
;; 	  "command": "node ./node_modules/gulp/bin/gulp.js default",
;; 	  "inspect-brk": true
;; 	}
;;   ]
;; }

;;; Available settings:
;;
;; "type": Type of runtime (currently "node" or "chrome" are supported).
;;
;; "root": Relative path to the root directory from where files are served.
;;         Alias: "webRoot".
;;
;; "sourceMapPathOverrides": Custom sourcemap mappings.  Maps source paths to
;;         locations on disk.
;;
;;         Default value:
;;
;;         {
;;           "webpack:///./~/": "${root}/node_modules/",
;;           "webpack:///./":   "${root}/",
;;           "webpack:///":     "/",
;;           "webpack:///src/": "${root}/"
;;         }
;;
;; Chrome-specific settings:
;;
;; "host": Host on which Chrome is running (defaults to "localhost").
;;
;; "port": Port on which Chrome is running (defaults to 9222).
;;
;; "url": Url to open when running `indium-launch-chrome'.
;;
;; Nodejs-specific settings:
;;
;; "command": Nodejs command to start a new process.  The `--inspect' flag will
;;            be added automatically.
;;
;; "inspect-brk": Whether Indium should break at the first statement (true by
;;            default).
;;
;; "host":    Host on which the Node inspector is listening (defaults to "localhost").
;;
;; "port": Port on which the Node inspector is listening (defaults to 9229).

;;; Code:


(require 'url)
(require 'seq)
(require 'map)
(require 'subr-x)
(require 'json)

(require 'indium-structs)
(require 'indium-backend)

(declare-function indium-connection-nodejs-p "indium-nodejs.el")


(defvar indium-workspace-filename ".indium.json"
  "Name of the configuration file containing the Indium project settings.")

(defvar indium-workspace-configuration nil
  "Configuration in the settings file used for connecting.
Do not set this variable directly.")

(defmacro with-indium-workspace-configuration (&rest body)
  "Promt the users for a configuration and evaluate BODY.
During the evaluation of BODY, `indium-workspace-configuration'
is set to the choosen configuration."
  (declare (indent 0) (debug t))
  `(progn
     (unless indium-workspace-configuration
       (indium-workspace-read-configuration))
     ,@body))

(defun indium-workspace-root ()
  "Lookup the root workspace directory from the current buffer.

If a connection is already open, return the `project-root' stored
in that connection.

If no connection is open yet, lookup the root directory as follows:

  - The root directory is specified by the \"webRoot\" (alias
    \"root\") configuration option.

  - If no \"root\" option is set, it defaults to the directory
    containing the \".indium.json\" project file.

If the root directory does not exist, signal an error."
  (let ((root (or (indium-current-connection-project-root)
		  (indium-workspace--root-from-configuration)
		  (indium-workspace--project-directory))))
    (unless (file-directory-p root)
      (user-error "Project root directory does not exist"))
    root))

(defun indium-workspace--root-from-configuration ()
  "Return the root directory read from the project configuration.
If no root is specified, return nil."
  (when-let ((root (or (map-elt indium-workspace-configuration 'root)
		       (map-elt indium-workspace-configuration 'webRoot))))
    (expand-file-name root (indium-workspace--project-directory))))

(defun indium-workspace--project-directory ()
  "Return the directory containing the \".indium.json\" file."
  (locate-dominating-file default-directory
			  indium-workspace-filename))

(defun indium-workspace-ensure-setup ()
  "Signal an error no workspace file can be found."
  (unless (indium-workspace--project-directory)
    (error "No file .indium.json found in the current project")))

(defun indium-workspace-settings-file ()
  "Lookup the filename of the settings file for the current workspace.
Return nil if not found."
  (when-let ((dir (indium-workspace--project-directory)))
    (expand-file-name indium-workspace-filename
		      dir)))

(defun indium-workspace-settings ()
  "Return the workspace settings read from the workspace file."
  (indium-workspace-ensure-setup)
  (with-temp-buffer
    (insert-file-contents (indium-workspace-settings-file))
    (goto-char (point-min))
    (ignore-errors
      (json-read))))

(defun indium-workspace-read-configuration ()
  "Prompt for the configuration used for connecting to a backend.
Set `indium-workspace-configuration' to the choosen configuration.
If the settings file contains only one configuration, set it."
  (let* ((settings (indium-workspace-settings))
	 (configurations (map-elt settings 'configurations))
	 (configuration-names (seq-map (lambda (configuration)
					 (map-elt configuration 'name))
				       configurations)))
    (unless configurations
      (user-error "No configuration provided in the project file"))
    (setq indium-workspace-configuration
	  (if (= (seq-length configurations) 1)
	      (seq-elt configurations 0)
	    (let ((name (completing-read "Choose a configuration: "
					 configuration-names
					 nil
					 t)))
	      (seq-find (lambda (configuration)
			  (string-equal (map-elt configuration 'name)
					name))
			configurations))))))



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
  (indium-workspace-ensure-setup)
  (let* ((root (indium-workspace-root))
	 (path (seq-drop (car (url-path-and-query
			       (url-generic-parse-url url)))
			 1))
	 (file (expand-file-name path root)))
    (when (or ignore-existence (file-regular-p file))
      file)))

(defun indium-workspace-make-url (file)
  "Return the url associated with the local FILE."
  (or (indium-workspace--make-url-using-file-path file)
      (indium-workspace--make-url-using-file-protocol file)
      (indium-workspace--make-url-using-workspace file)))

(defun indium-workspace--make-url-using-file-path (file)
  "When using nodejs, the path of FILE should be used directly."
  (when (indium-connection-nodejs-p indium-current-connection)
    (convert-standard-filename file)))

(defun indium-workspace--make-url-using-file-protocol (file)
  "Return a url using the \"file://\" protocol for FILE.
If the current connection doesn't use the file protocol, return nil."
  (when (indium-workspace--file-protocol-p)
    (format "file://%s" file)))

(defun indium-workspace--make-url-using-workspace (file)
  "Return the url associated with the local FILE.
The url is built using `indium-workspace-root'."
  (indium-workspace-ensure-setup)
  (let* ((root (indium-workspace-root))
	 (url (indium-workspace--url-basepath (indium-current-connection-url)))
         (path (file-relative-name file root)))
    (setf (url-filename url) (indium-workspace--absolute-path path))
    (url-recreate-url url)))

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

(provide 'indium-workspace)
;;; indium-workspace.el ends here
