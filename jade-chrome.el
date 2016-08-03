;;; jade-chrome.el --- Chrom{e|ium} support for jade  -*- lexical-binding: t; -*-

;; Copyright (C) 2016  Nicolas Petton

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

;; Handle jade connections to Chrom{e|ium} using the webkit backend.
;;
;; To open a Jade connection, enable chromium/chrome remote debugging:
;;
;;     chromium --remote-debugging-port=9222 https://gnu.org

;;; Code:

(require 'url)
(require 'json)
(require 'map)
(require 'seq)

(require 'jade-webkit)

(defun jade-connect-to-chrome (host port)
  "Open a connection to a webkit tab on HOST:PORT."
    (interactive (list (read-from-minibuffer "Host: " "127.0.0.1")
                       (read-from-minibuffer "Port: " "9222")))
  (jade-chrome--get-tabs-data host port #'jade-chrome--connect-to-tab))

(defun jade-chrome--get-tabs-data (host port callback)
  "Get the list of open tabs on HOST:PORT and evaluate CALLBACK with it."
  (url-retrieve (format "http://%s:%s/json" host port)
                (lambda (_status)
                  ;; TODO: handle errors
                  (funcall callback (jade-chrome--read-tab-data)))))

(defun jade-chrome--connect-to-tab (tabs)
  "Ask the user for a tab in the list TABS and connects to it."
  (let* ((urls (seq-map (lambda (tab)
                            (map-elt tab 'url))
                          tabs))
         (url (completing-read "Tab: " urls nil t))
         (tab (seq-find (lambda (tab)
                          (string= (map-elt tab 'url) url))
                        tabs))
         (websocket-url (map-elt tab 'webSocketDebuggerUrl)))
    (jade-webkit--open-ws-connection url websocket-url)))

(defun jade-chrome--read-tab-data ()
  "Return the JSON tabs data in the current buffer."
  (when (save-match-data
          (looking-at "^HTTP/1\\.1 200 OK$"))
    (goto-char (point-min))
    (search-forward "\n\n")
    (delete-region (point-min) (point))
    (json-read)))

(provide 'jade-chrome)
;;; jade-chrome.el ends here
