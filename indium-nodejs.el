;;; indium-nodejs.el --- NodeJS support for indium  -*- lexical-binding: t; -*-

;; Copyright (C) 2016-2018  Nicolas Petton

;; Author: Nicolas Petton <nicolas@petton.fr>
;; Keywords: tools, javascript

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

;; Handle indium connections to a NodeJS process using the v8 backend.
;;
;; Important note: For this package to work, NodeJS version 7.0 (or any newer
;; version) is required.

;;; Code:

(require 'url)
(require 'url-parse)
(require 'json)
(require 'map)
(require 'seq)
(require 'subr-x)

(require 'indium-v8)
(require 'indium-workspace)

(defgroup indium-nodejs nil
  "Indium NodeJS."
  :prefix "indium-nodejs-"
  :group 'indium)

(defcustom indium-nodejs-default-inspect-brk t
  "When non-nil, break the execution at the first statement."
  :type 'boolean)

(defcustom indium-nodejs-default-host
  "localhost"
  "Default NodeJS remote debugger host."
  :type 'string)

(defcustom indium-nodejs-default-port
  9229
  "Default NodeJS remote debugger port."
  :type 'integer)

(defun indium-nodejs--command ()
  "Return the command to be run to start a node process.
The command is read from the workspace configuration file."
  (let ((command (map-elt indium-workspace-configuration 'command)))
    (unless command
      (user-error "No NodeJS command specified in the .indium.json file"))
    command))

(defun indium-launch-nodejs ()
  "Start a NodeJS process.

Execute the command based on `indium-nodejs--command', adding the
`--inspect' flag.  When the process is ready, open an Indium
connection on it.

If `indium-nodejs--inspect-brk' is set to non-nil, break the
execution at the first statement."
  (let* ((default-directory (indium-workspace-root))
	 (process (make-process :name "indium-nodejs-process"
				:buffer "*node process*"
				:filter #'indium-nodejs--process-filter
				:command (list shell-file-name
					       shell-command-switch
					       (indium-nodejs--command-with-flags)))))
    (switch-to-buffer (process-buffer process))))

(defun indium-connect-to-nodejs ()
  "Open a connection to an existing NodeJS process."
  (let* ((host (indium-nodejs--host))
	 (port (indium-nodejs--port))
	 (default-directory (indium-workspace-root)))
    (indium-nodejs--get-process-id host
				   port
				   (lambda (id)
				     (indium-nodejs--connect host port id)))))


(defun indium-nodejs--host ()
  "Return the debugging host for a NodeJS process.
The host is either read from the workpace configuration file or
`indium-nodejs-default-host'."
  (map-elt indium-workspace-configuration 'host indium-nodejs-default-host))

(defun indium-nodejs--port ()
  "Return the debugging port for a NodeJS process.
The port is either read from the workpace configuration file or
`indium-nodejs-default-port'."
  (map-elt indium-workspace-configuration 'port indium-nodejs-default-port))

(defun indium-nodejs--inspect-brk ()
  "Return non nil if the option `--inspect-brk' should be used.
The setting is either read from the workpace configuration file or
`indium-nodejs-default-inspect-brk'."
  (map-elt indium-workspace-configuration 'inspect-brk))


(defun indium-nodejs--get-process-id (host port callback)
  "Get the id of the websocket path at HOST:PORT and evaluate CALLBACK with it."
  (url-retrieve (format "http://%s:%s/json/list" host port)
		(lambda (status)
		  (funcall callback (if (eq :error (car status))
					nil
				      (indium-nodejs--read-process-id))))))

(defun indium-nodejs--read-process-id ()
  "Return the process id read from the JSON data in the current buffer."
    (when (save-match-data
          (looking-at "^HTTP/.* 200 OK$"))
    (goto-char (point-min))
    (search-forward "\n\n")
    (delete-region (point-min) (point))
    (map-elt (seq-elt (json-read) 0) 'id)))


(defun indium-nodejs--connect (host port path &optional process)
  "Ask the user for a websocket url HOST:PORT/PATH and connects to it.
When PROCESS is non-nil, attach it to the connection."
  (indium-maybe-quit)
  (unless indium-current-connection
    (let ((websocket-url (format "ws://%s:%s/%s" host port path))
          (url (format "file://%s" default-directory)))
      (indium-v8--open-ws-connection url
				     websocket-url
				     (when process
				       (lambda ()
					 (setf (indium-current-connection-process) process)))
				     t))))

(defun indium-nodejs--command-with-flags ()
  "Return the command to be run with the `--inspect' or `--inspect-brk' flag."
  (let ((command (indium-nodejs--command))
	(inspect-flag (if (indium-nodejs--inspect-brk)
			  "--inspect-brk"
			"--inspect")))
    (if (string-match "\\<node\\>" command)
	(replace-match (concat "node " inspect-flag) nil nil command)
      (user-error "Invalid command specified"))))

(defun indium-nodejs--process-filter (process output)
  "Filter function for PROCESS.
Append OUTPUT to the PROCESS buffer, and parse it to detect the
socket URL to connect to."
  ;; Append output to the process buffer
  (with-current-buffer (process-buffer process)
    (goto-char (point-max))
    (insert output))
  (when (string-match-p "Debugger listening on" output)
    (ignore-errors
      (indium-nodejs--connect-to-process process output))))

(defun indium-nodejs--connect-to-process (process output)
  "If PROCESS OUTPUT contain the WS url, connect to it."
  (save-match-data
    (string-match "://.*/\\(.*\\)$" output)
    (when-let ((path (match-string 1 output)))
      (indium-nodejs--connect "127.0.0.1" "9229" path process))))

(provide 'indium-nodejs)
;;; indium-nodejs.el ends here
