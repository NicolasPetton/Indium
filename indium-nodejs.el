;;; indium-nodejs.el --- NodeJS support for indium  -*- lexical-binding: t; -*-

;; Copyright (C) 2016-2017  Nicolas Petton

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
;; The nodejs process must be started with the `--inspect' flag:
;;
;;     node --inspect myfile.js
;;     node --inspect=localhost:9876 myfile.js
;;
;; To break on the first line of the application code, provide the --debug-brk
;; flag in addition to --inspect.
;;
;; You can optionally use `indium-run-node' to start a node process, in which
;; case the `--inspect' and `--debug-brk' flags will be added automatically.
;;
;; Important note: For this package to work, NodeJS version 7.0 (or any newer
;; version) is required.

;;; Code:

(require 'url)
(require 'json)
(require 'map)
(require 'seq)
(require 'subr-x)

(require 'indium-v8)

(declare-function indium-repl-get-buffer "indium-repl.el")

(defgroup indium-nodejs nil
  "Indium NodeJS."
  :prefix "indium-nodejs-"
  :group 'indium)

(defcustom indium-nodejs-inspect-brk nil
  "When non-nil, break the execution at the first statement.")

(defvar indium-nodejs-commands-history nil
  "Nodejs commands history.")

;;;###autoload
(defun indium-run-node (command)
  "Start a NodeJS process.

Execute COMMAND, adding the `--inspect' flag.  When the process
is ready, open an Indium connection on it.

If `indium-nodejs-inspect-brk' is set to non-nil, break the
execution at the first statement.

If a connection is already open, close it."
  (interactive (list (read-shell-command "Node command: "
                                         (or (car indium-nodejs-commands-history) "node ")
                                         'indium-nodejs-commands-history)))
  (indium-maybe-quit)
  (unless indium-current-connection
    (let ((process (make-process :name "indium-nodejs-process"
				 :buffer "*node process*"
				 :filter #'indium-nodejs--process-filter
				 :command (list shell-file-name
						shell-command-switch
						(indium-nodejs--add-flags command)))))
      (switch-to-buffer (process-buffer process)))))

;;;###autoload
(defun indium-restart-node ()
  "Restart the current nodejs process, and connect to it.

If no process has been started, or if it was not started using
`indium-run-node', do nothing."
  (interactive)
  (if-let ((connection indium-current-connection)
	   (command (car indium-nodejs-commands-history))
	   ;; Don't do anything when not a nodejs connection
	   (nodejs (map-elt (indium-current-connection-props) 'nodejs))
	   (process (indium-current-connection-process)))
      ;; Make sure we are in the same directory as the current connection.
      ;; TODO: set the directory in the connection directly instead
      ;; of relying on the REPL buffer
      (let ((default-directory (with-current-buffer (indium-repl-get-buffer)
				 default-directory)))
	(indium-quit)
	(indium-run-node command))
    (user-error "Start a NodeJS connection with `indium-run-node' first")))

;;;###autoload
(defun indium-connect-to-nodejs ()
  "Open a connection to host:port/path."
  (interactive)
  (let ((host (read-from-minibuffer "Host: " "127.0.0.1"))
        (port (read-from-minibuffer "Port: " "9229"))
        (path (read-from-minibuffer "Path: ")))
    (indium-nodejs--connect host port path)))

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

(defun indium-nodejs--add-flags (command)
  "Return COMMAND with the `--inspect' or `--inspect-brk' flag added."
  (let* ((tokens (split-string command))
         (program (car tokens))
         (arguments (cdr tokens))
	 (inspect-flag (if indium-nodejs-inspect-brk
			   "--inspect-brk"
			 "--inspect"))
         (result `(,program ,inspect-flag ,@arguments)))
  (mapconcat #'identity result " ")))

(defun indium-nodejs--process-filter (process output)
  "Filter function for PROCESS.
Append OUTPUT to the PROCESS buffer, and parse it to detect the
socket URL to connect to."
  ;; Append output to the process buffer
  (with-current-buffer (process-buffer process)
    (goto-char (point-max))
    (insert output))
  (ignore-errors
    (indium-nodejs--connect-to-process process output)))

(defun indium-nodejs--connect-to-process (process output)
  "If PROCESS OUTPUT contain the WS url, connect to it."
  (save-match-data
    (string-match "://.*/\\(.*\\)$" output)
    (when-let ((path (match-string 1 output)))
      (indium-nodejs--connect "127.0.0.1" "9229" path process))))

(provide 'indium-nodejs)
;;; indium-nodejs.el ends here
