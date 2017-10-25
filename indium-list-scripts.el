;;; indium-list-scripts.el --- List parsed scripts   -*- lexical-binding: t; -*-

;; Copyright (C) 2017  Nicolas Petton

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

;;; Code:
(require 'indium-script)
(require 'indium-structs)

(require 'map)
(require 'tabulated-list)

;;;###autoload
(defun indium-list-scripts ()
  "Display a list of parsed scripts."
  (interactive)
  (unless-indium-connected
    (user-error "Connect Indium to a runtime first"))
  (let ((buf (get-buffer-create "*Indium scripts*")))
    (with-current-buffer buf
      (indium-list-scripts-mode)
      (indium-list-scripts--refresh)
      (tabulated-list-print))
    (display-buffer buf)))

(define-derived-mode indium-list-scripts-mode tabulated-list-mode "Indium list scripts"
  "Major mode for listing parsed JavaScript scripts."
  (setq tabulated-list-format [("Script source" 0 t)])
  (add-hook 'tabulated-list-revert-hook 'indium-list-scripts--refresh nil t)
  (tabulated-list-init-header))

(defun indium-list-scripts--refresh ()
  "Refresh the list of parsed scripts."
  (setq tabulated-list-entries
	(map-apply (lambda (_ script)
		     (indium-list-scripts--make-entry script))
		   (indium-current-connection-scripts))))

(defun indium-list-scripts--make-entry (script)
  "Return a tabulated list entry for SCRIPT."
  (list (indium-script-id script)
	(make-vector 1 (if-let ((file (indium-script-get-file script)))
			   (cons (indium-script-url script)
				 (list 'action (lambda (&rest _)
						 (find-file file))))
			 (indium-script-url script)))))

(provide 'indium-list-scripts)
;;; indium-list-scripts.el ends here
