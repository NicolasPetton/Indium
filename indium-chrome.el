;;; indium-chrome.el --- Chrom{e|ium} support for indium  -*- lexical-binding: t; -*-

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

;; Handle indium connections to Chrom{e|ium} using the v8 backend.

;;; Code:


(require 'url)
(require 'json)
(require 'map)
(require 'seq)

(declare-function indium-client-connect "indium-client.el")

(defgroup indium-chrome nil
  "Chrome interaction."
  :prefix "indium-chrome-"
  :group 'indium)

(defun indium-chrome--default-executable ()
  "Return a default executable based on the OS."
  (cond ((string-equal system-type "darwin")
	 "/Applications/Google Chrome.app/Contents/MacOS/Google Chrome")
	((string-equal system-type "windows-nt")
	 "C:/Program Files (x86)/Google/Chrome/Application/chrome.exe")
	(t "chromium")))

(defcustom indium-chrome-executable
  (indium-chrome--default-executable)
  "Chrome executable."
  :type '(file))

(defcustom indium-chrome-default-port
  9222
  "Default Chrome remote debugger port."
  :type '(integer))

(defun indium-launch-chrome (conf)
  "Start chrome/chromium with remote debugging enabled based on CONF settings."
  (let-alist conf
    (unless .url
      (error "No url specified in configuration"))
    (make-process :name "indium-chrome-process"
                  :command (list (indium-chrome--find-executable)
				 (format "--remote-debugging-port=%s"
					 (or .port indium-chrome-default-port))
				 .url))
    (indium-client-connect (file-name-directory .projectFile) .name)))

(defun indium-chrome--find-executable ()
  "Find chrome executable using `indium-chrome-executable'."
  (let ((executable (executable-find indium-chrome-executable)))
    (unless executable
      (user-error "Cannot find chrome/chromium binary (%s) in PATH" indium-chrome-executable))
    executable))

(provide 'indium-chrome)
;;; indium-chrome.el ends here
