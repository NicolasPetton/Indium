;;; indium-list-scripts.el --- List script and sourcemap mappings   -*- lexical-binding: t; -*-

;; Copyright (C) 2017-2018  Nicolas Petton

;; Author: Nicolas Petton <nicolas@petton.fr>

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
;;
;; This file provides commands useful for debugging project configuration
;; issues when breakpoints or sourcemaps do not work.
;;
;; - `indium-list-sourcemap-sources': List all sourcemap sources, as resolved to
;; disk files.  This commands helps understanding how Indium maps sourcemaps to
;; file paths using the `.indium.json' project file.
;;
;; - `indium-list-script-sources': List all script parsed by the backend.  Their
;; source file is resolved to a file on disk when possible.

;;; Code:


(require 'indium-client)

(require 'map)
(require 'tabulated-list)

(defvar indium-list-sources-function nil
  "Function used to fetch a list of sources.")

(make-local-variable 'indium-list-sources-function)

;;;###autoload
(defun indium-list-sourcemap-sources ()
  "Display a list of all resolved sourcemap sources."
  (interactive)
  (let ((buf (get-buffer-create (indium-list-sources-buffer-name))))
    (with-current-buffer buf
      (setq indium-list-sources-function #'indium-client-get-sourcemap-sources)
      (indium-list-sources-mode)
      (indium-list-sources--refresh))
    (display-buffer buf)))

;;;###autoload
(defun indium-list-script-sources ()
  "Display a list of all resolved script sources."
  (interactive)
  (let ((buf (get-buffer-create (indium-list-sources-buffer-name))))
    (with-current-buffer buf
      (setq indium-list-sources-function #'indium-client-get-script-sources)
      (indium-list-sources-mode)
      (indium-list-sources--refresh))
    (display-buffer buf)))

(defun indium-list-sources-buffer-name ()
  "Return the name of the buffer used to list sources."
   "*Indium sources*")

(define-derived-mode indium-list-sources-mode tabulated-list-mode "Indium list sources"
  "Major mode for listing sources."
  (setq tabulated-list-format [("sources" 0 t)])
  (add-hook 'tabulated-list-revert-hook 'indium-list-sources--refresh nil t)
  (tabulated-list-init-header))

(defun indium-list-sources--refresh ()
  "Refresh the list of parsed scripts."
  (funcall indium-list-sources-function
   (lambda (sources)
     (with-current-buffer  (get-buffer (indium-list-sources-buffer-name))
       (setq tabulated-list-entries
	     (seq-map (lambda (source)
			(indium-list-sources--make-entry source))
		      (seq-filter #'identity sources)))
       (tabulated-list-print)))))

(defun indium-list-sources--make-entry (source)
  "Return a tabulated list entry for SOURCE."
  (list nil
	(make-vector 1 (if (file-exists-p source)
			   (cons source
				 (list 'action (lambda (&rest _)
						 (find-file source))))
			 (propertize source 'font-lock-face 'error)))))

(provide 'indium-list-sources)
;;; indium-list-sources.el ends here
