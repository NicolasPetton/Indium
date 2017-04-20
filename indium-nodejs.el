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
;; Important note: For this package to work, NodeJS version 7.0 (or any newer
;; version) is required.

;;; Code:

(require 'url)
(require 'json)
(require 'map)
(require 'seq)

(require 'indium-v8-inspector)

(defun indium-connect-to-nodejs ()
  "Open a connection to a webkit tab on host:port/path."
  (interactive)
  (when (or (null indium-connection)
            (yes-or-no-p "This requires closing the current connection.  Are you sure? "))
    (when indium-connection
      (indium-quit))
    (let ((host (read-from-minibuffer "Host: " "127.0.0.1"))
          (port (read-from-minibuffer "Port: " "9229"))
          (path (read-from-minibuffer "Path: ")))
      (indium-nodejs--connect host port path))))

(defun indium-nodejs--connect (host port path)
  "Ask the user for a tab in the list TABS and connects to it."
  (let ((url (format "ws://%s:%s/%s" host port path)))
    (indium-v8-inspector--open-ws-connection url)))

(provide 'indium-nodejs)
;;; indium-nodejs.el ends here
