;;; indium-chrome.el --- Chrom{e|ium} support for indium  -*- lexical-binding: t; -*-

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

;; Handle indium connections to Chrom{e|ium} using the v8 backend.
;;
;; To open a Indium connection, enable chromium/chrome remote debugging:
;;
;;     chromium --remote-debugging-port=9222 https://gnu.org

;;; Code:

(require 'url)
(require 'json)
(require 'map)
(require 'seq)

(require 'indium-v8)
(require 'indium-workspace)

(eval-and-compile (require 'indium-structs))

(defgroup indium-chrome nil
  "Chrome interaction."
  :prefix "indium-chrome-"
  :group 'indium)

(defun indium-chrome--default-executable ()
  "Return a default executable based on the OS."
  (if (string-equal system-type "darwin")
      "/Applications/Google Chrome.app/Contents/MacOS/Google Chrome"
    "chromium"))

(defcustom indium-chrome-executable
  (indium-chrome--default-executable)
  "Chrome executable."
  :type '(file))

(defcustom indium-chrome-port
  9222
  "Chrome remote debugger port."
  :type '(integer))

(defvar indium-chrome-url-history nil
  "Chrome urls history.")

;;;###autoload
(defun indium-run-chrome (url)
  "Start chrome/chromium with remote debugging enabled.
Open URL if provided."
  (interactive (list (completing-read "Url: "
                                      nil
                                      nil
                                      nil
                                      nil
                                      'indium-chrome-url-history
                                      (car indium-chrome-url-history))))
  (make-process :name "indium-chrome-process"
                :command (list (indium-chrome--find-executable)
                               (format "--remote-debugging-port=%s" indium-chrome-port)
                               (or url "")))
  (message "Connecting to Chrome instance...")
  (indium-chrome--try-connect "127.0.0.1" 10))

(defun indium-chrome--find-executable ()
  "Find chrome executable using `indium-chrome-executable'."
  (let ((executable (executable-find indium-chrome-executable)))
    (unless executable
      (user-error "Cannot find chrome/chromium binary (%s) in PATH" indium-chrome-executable))
    executable))


(defun indium-chrome--try-connect (host num-tries)
  "Try to connect to chrome on HOST.
Try a maximum of NUM-TRIES."
  (message "Trying to connect to the Chrome instance...")
  (sleep-for 1)
  (indium-chrome--get-tabs-data host
				indium-chrome-port
				(lambda (tabs)
				  (if tabs
				      (indium-chrome--connect-to-tab tabs)
				    (when (> num-tries 0)
				      (indium-chrome--try-connect host (1- num-tries)))))))

;;;###autoload
(defun indium-connect-to-chrome ()
  "Open a connection to a v8 tab."
  (interactive)
  (when (or (null indium-current-connection)
            (yes-or-no-p "This requires closing the current connection.  Are you sure? "))
    (when-indium-connected
      (indium-quit))
    (let ((host (read-from-minibuffer "Host: " "127.0.0.1"))
          (port (read-from-minibuffer "Port: " (number-to-string indium-chrome-port))))
      (indium-chrome--get-tabs-data host port #'indium-chrome--connect-to-tab))))

(defun indium-chrome--get-tabs-data (host port callback)
  "Get the list of open tabs on HOST:PORT and evaluate CALLBACK with it."
  (url-retrieve (format "http://%s:%s/json" host port)
                (lambda (status)
                  (funcall callback (if (eq :error (car status))
                                        nil
                                      (indium-chrome--read-tab-data))))))

(defun indium-chrome--connect-to-tab (tabs)
  "Connects to a tab in the list TABS.
If there are more then one tab available ask the user which tab to connect."
  (unless tabs
    (error "No Chrome tab found.  Is Chrome running with the `--remote-debugging-port' flag set?"))
  (if (= (seq-length tabs) 1)
      (indium-chrome--connect-to-tab-with-url (map-elt (seq-elt tabs 0) 'url) tabs)
    (let* ((urls (seq-map (lambda (tab)
                            (map-elt tab 'url))
                          tabs))
           (url (completing-read "Tab: " urls nil t)))
      (indium-chrome--connect-to-tab-with-url url tabs))))

(defun indium-chrome--connect-to-tab-with-url (url tabs)
  "Connect to a tab with URL from list TABS."
  (let* ((tab (seq-find (lambda (tab)
                          (string= (map-elt tab 'url) url))
                        tabs))
         (websocket-url (map-elt tab 'webSocketDebuggerUrl))
         (workspace))
    ;; No need to setup a workspace when using the file protocol.
    (unless (string= (url-type (url-generic-parse-url url)) "file")
      (setq workspace (indium-workspace-read)))
    (indium-v8--open-ws-connection url websocket-url nil nil workspace)))

(defun indium-chrome--read-tab-data ()
  "Return the JSON tabs data in the current buffer."
  (when (save-match-data
          (looking-at "^HTTP/1\\.1 200 OK$"))
    (goto-char (point-min))
    (search-forward "\n\n")
    (delete-region (point-min) (point))
    (json-read)))

(provide 'indium-chrome)
;;; indium-chrome.el ends here
