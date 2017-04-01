;;; jade-debugger-locals.el --- Inspect locals       -*- lexical-binding: t; -*-

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

(require 'jade-render)

(declare 'jade-backend-get-properties)
(declare 'jade-debugger-get-scopes-properties)
(declare 'jade-debugger-get-current-scopes)

(defun jade-debugger-locals (&optional no-pop)
  "Inspect the local variables in the current stack frame's scope.
Unless NO-POP is non-nil, pop the locals buffer."
  (interactive)
  (let* ((buf (jade-debugger-locals-get-buffer-create))
         (inhibit-read-only t)
         (scopes (jade-debugger-get-current-scopes)))
    (with-current-buffer buf
      (erase-buffer))
    (jade-debugger-get-scopes-properties
     scopes
     (lambda (properties scope)
       (jade-debugger-locals-render-properties properties scope no-pop)))))

(defun jade-debugger-locals-maybe-refresh ()
  "When a local inspector is open, refresh it."
  (interactive)
  (let ((buf (jade-debugger-locals-get-buffer)))
    (when buf
      (jade-debugger-locals t))))

(defun jade-debugger-locals-render-properties (properties scope &optional no-pop)
  "Render PROPERTIES in SCOPE.
Unless NO-POP in non-nil, pop the locals buffer."
  (let* ((buf (jade-debugger-locals-get-buffer-create))
         (inhibit-read-only t)
         (name (map-elt scope 'name))
         (type (map-elt scope 'type))
         (description (if (or (null name)
                              (string= name "undefined"))
                          type
                        name)))
    (with-current-buffer buf
      (save-excursion
        (goto-char (point-max))
        (jade-render-keyword description)
        (insert "\n\n")
        (jade-render-properties properties)
        (insert "\n")))
    (unless no-pop
      (pop-to-buffer buf))))

(defun jade-debugger-locals-get-buffer ()
  "Return the buffer to use to inspect locals."
  (get-buffer (jade-debugger-locals-buffer-name)))

(defun jade-debugger-locals-buffer-name ()
  "Return the name of the buffer to use to inspect locals."
  "*JS Debugger Locals*")

(defun jade-debugger-locals-get-buffer-create ()
  "Create a locals buffer unless one exists, and return it."
  (let ((buf (jade-debugger-locals-get-buffer)))
    (unless buf
      (setq buf (generate-new-buffer (jade-debugger-locals-buffer-name)))
      (jade-debugger-locals-setup-buffer buf))
    buf))

(defun jade-debugger-locals-setup-buffer (buffer)
  "Setup BUFFER."
  (with-current-buffer buffer
    (jade-debugger-locals-mode)
    (read-only-mode)))

(defvar jade-debugger-locals-mode-map
  (let ((map (copy-keymap jade-inspector-mode-map)))
    (define-key map "g" nil)
    (define-key map "l" nil)
    map))

(define-derived-mode jade-debugger-locals-mode jade-inspector-mode "Locals"
  "Major mode for inspecting local variables in a scope-chain.

\\{jade-debugger-locals-mode-map}")

(provide 'jade-debugger-locals)
;;; jade-debugger-locals.el ends here
