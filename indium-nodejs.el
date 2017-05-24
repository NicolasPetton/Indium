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

;; Handle indium connections to a NodeJS process using the webkit backend.
;; The nodejs process must be started with the `--inspect' flag:
;;
;;     node --inspect myfile.js
;;     node --inspect=localhost:9876 myfile.js
;;
;; To break on the first line of the application code, provide the --debug-brk
;; flag in addition to --inspect.
;;
;; You can optionally use `indium-run-nodejs' to start a node process, in which
;; case the `--inspect' and `--debug-brk' flags will be added automatically.
;;
;; Important note: For this package to work, NodeJS version 7.0 (or any newer
;; version) is required.

;;; Code:

(require 'url)
(require 'json)
(require 'map)
(require 'seq)
(require 'exec-path-from-shell)

(require 'indium-webkit)

(defvar indium-nodejs-commands-history nil
  "Nodejs commands history.")

(defvar indium-exec-path-setup nil
  "Whether the exec path has been set up.")

(defun indium-run-node (command)
  "Start a NodeJS process.
Execute COMMAND, adding the `--inspect' and `--debug-brk' flags.
When the process is ready, open an Indium connection on it."
  (interactive (list (read-shell-command "Node command: "
                                         (or (car indium-nodejs-commands-history) "node ")
                                         'indium-nodejs-commands-history)))
  (indium--setup-exec-path)
  (let ((process (make-process :name "indium-nodejs-process"
                               :buffer "*node process*"
                               :filter #'indium-nodejs--process-filter
                               :command (list shell-file-name
                                              shell-command-switch
                                              (indium-nodejs--add-flags command)))))
    (switch-to-buffer (process-buffer process))))

(defun indium-connect-to-nodejs ()
  "Open a connection to a webkit tab on host:port/path."
  (interactive)
  (let ((host (read-from-minibuffer "Host: " "127.0.0.1"))
        (port (read-from-minibuffer "Port: " "9229"))
        (path (read-from-minibuffer "Path: ")))
    (indium-nodejs--connect host port path)))

(defun indium-nodejs--connect (host port path)
  "Ask the user for a websocket url HOST:PORT/PATH and connects to it."
  (when (or (null indium-connection)
            (yes-or-no-p "This requires closing the current connection.  Are you sure? "))
    (when indium-connection
      (indium-quit))
    (let ((websocket-url (format "ws://%s:%s/%s" host port path))
          (url (format "file://%s" default-directory)))
      (indium-webkit--open-ws-connection url websocket-url nil t))))

(defun indium-nodejs--add-flags (command)
  "Return COMMAND with the `--inspect' `--debug-brk' flags added."
  (let* ((tokens (split-string command))
         (program (car tokens))
         (arguments (cdr tokens))
         (result `(,program "--inspect" "--debug-brk" ,@arguments)))
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
    (indium-nodejs--connect-to-process output)))

(defun indium-nodejs--connect-to-process (output)
  "If OUTPUT contain the WS url, connect to it."
  (save-match-data
    (string-match "devtools://.*/\\(.*\\)$" output)
    (when-let ((path (match-string 1 output)))
      (indium-nodejs--connect "127.0.0.1" "9229" path))))

(defun indium--setup-exec-path ()
  "Setup the exec path using `exec-path-from-shell'.

This ensures that the nodejs binary used by Emacs will be the
same as the one from the user's shell."
  (unless indium-exec-path-setup
    (exec-path-from-shell-initialize)
    (setq indium-exec-path-setup t)))

(provide 'indium-nodejs)
;;; indium-nodejs.el ends here
