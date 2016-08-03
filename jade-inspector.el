;;; jade-inspector.el --- Inspector for JavaScript objects  -*- lexical-binding: t; -*-

;; Copyright (C) 2016  Nicolas Petton

;; Author: Nicolas Petton <nicolas@petton.fr>
;; Keywords: convenience, tools, javascript

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

(require 'seq)
(require 'map)
(require 'jade-render)
(require 'jade-faces)

(defvar jade-inspector-history nil)
(make-variable-buffer-local 'jade-inspector-history)

(declare jade-backend-get-properties)

(defun jade-inspector-inspect (reference)
  "Open an inspector on the remote object REFERENCE."
  (let ((objectid (map-elt reference 'objectid)))
    (if objectid
        (jade-backend-get-properties (jade-backend)
                                     objectid
                                     (lambda (properties)
                                       (jade-inspector--inspect-properties properties reference)))
      (message "Cannot inspect %S" (map-elt reference 'description)))))

(defun jade-inspector--inspect-properties (properties reference)
  (let ((buf (jade-inspector-get-buffer-create))
        (inhibit-read-only t))
    (with-current-buffer buf
      (jade-inspector-push-to-history reference)
      (save-excursion
        (erase-buffer)
        (jade-render-keyword (jade-description-string reference t))
        (insert "\n\n")
        (jade-render-properties properties)))
    (pop-to-buffer buf)))

(defun jade-inspector-keybinding (command)
  (key-description (car (where-is-internal command))))

(defun jade-inspector-pop ()
  (interactive)
  (if (cdr jade-inspector-history)
      (progn
        (pop jade-inspector-history)
        (funcall #'jade-inspector-inspect (car jade-inspector-history)))
    (message "No previous object to inspect")))

(defun jade-inspector-goto-reference (direction)
  (let* ((delta (pcase direction
                  (`next 1)
                  (`previous -1)))
         (reference (save-excursion
                      (forward-line delta)
                      (when (eq direction 'previous)
                        (end-of-line))
                      (while (and (not (eobp))
                                  (not (get-text-property (point) 'jade-reference)))
                        (forward-char delta))
                      (when (get-text-property (point) 'jade-reference)
                        (point)))))
    (when reference
      (goto-char reference)
      ;; go to the first char of the reference
      (while (get-text-property (point) 'jade-reference)
        (backward-char 1))
      (forward-char 1))))

(defun jade-inspector-next-reference ()
  (interactive)
  (jade-inspector-goto-reference 'next))

(defun jade-inspector-previous-reference ()
  (interactive)
  (jade-inspector-goto-reference 'previous))

(defun jade-inspector-refresh ()
  (interactive)
  (when jade-inspector-history
    (funcall #'jade-inspector-inspect (car jade-inspector-history))))

(defun jade-inspector-push-to-history (reference)
  (unless (string= (map-elt reference 'objectid)
                   (map-elt (car jade-inspector-history) 'objectid))
      (push reference jade-inspector-history)))

(defun jade-inspector-get-buffer ()
  (get-buffer (jade-inspector-buffer-name)))

(defun jade-inspector-get-buffer-create ()
  "Return an inspector buffer for the current connection.
If no buffer exists, create one."
  (let ((buf (jade-inspector-get-buffer)))
    (unless buf
      (setq buf (get-buffer-create (jade-inspector-buffer-name)))
      (jade-inspector-setup-buffer buf jade-connection))
    buf))

(defun jade-inspector-setup-buffer (buffer connection)
  "Setup the inspector BUFFER for CONNECTION."
  (with-current-buffer buffer
    (jade-inspector-mode)
    (setq-local jade-connection connection)))

(defun jade-inspector-buffer-name ()
  "Return the inspector buffer name for the current connection."
  (concat "*JS Inspector " (map-elt jade-connection 'url) "*"))

(defvar jade-inspector-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map [return] #'jade-follow-link)
    (define-key map "\C-m" #'jade-follow-link)
    (define-key map [mouse-1] #'jade-follow-link)
    (define-key map "l" #'jade-inspector-pop)
    (define-key map "g" #'jade-inspector-refresh)
    (define-key map "n" #'jade-inspector-next-reference)
    (define-key map "p" #'jade-inspector-previous-reference)
    (define-key map [tab] #'jade-inspector-next-reference)
    (define-key map [backtab] #'jade-inspector-previous-reference)
    map))

(define-derived-mode jade-inspector-mode special-mode "Inspector"
  "Major mode for inspecting JavaScript objects.

\\{jade-inspector-mode-map}"
  (setq buffer-read-only t)
  (font-lock-ensure)
  (setq-local electric-indent-chars nil)
  (setq-local truncate-lines t))

(provide 'jade-inspector)
;;; jade-inspector.el ends here
