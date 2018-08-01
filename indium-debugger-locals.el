;;; indium-debugger-locals.el --- Inspect locals       -*- lexical-binding: t; -*-

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

;;; Code:

(require 'indium-render)
(require 'indium-inspector)

(declare-function indium-debugger-get-scopes-properties "indium-debugger.el")
(declare-function indium-debugger-get-current-scopes "indium-debugger.el")

(defun indium-debugger-locals (&optional no-pop)
  "Inspect the local variables in the current stack frame's scope.
Unless NO-POP is non-nil, pop the locals buffer."
  (interactive)
  (let* ((buf (indium-debugger-locals-get-buffer-create))
         (inhibit-read-only t)
         (scopes (indium-debugger-get-current-scopes)))
    (with-current-buffer buf
      (erase-buffer))
    (indium-debugger-get-scopes-properties
     scopes
     (lambda (properties scope)
       (indium-debugger-locals-render-properties properties scope no-pop)))))

(defun indium-debugger-locals-maybe-refresh ()
  "When a local inspector is open, refresh it."
  (interactive)
  (let ((buf (indium-debugger-locals-get-buffer)))
    (when buf
      (indium-debugger-locals t))))

(defun indium-debugger-locals-render-properties (properties scope &optional no-pop)
  "Render PROPERTIES in SCOPE.
Unless NO-POP in non-nil, pop the locals buffer."
  (let* ((buf (indium-debugger-locals-get-buffer-create))
         (inhibit-read-only t)
         (name (indium-scope-name scope))
         (type (indium-scope-type scope))
         (description (if (or (null name)
                              (string= name "undefined"))
                          type
                        name)))
    (with-current-buffer buf
      (save-excursion
        (goto-char (point-max))
        (indium-render-keyword description)
        (insert "\n\n")
        (indium-inspector--insert-sorted-properties properties)))
    (unless no-pop
      (pop-to-buffer buf))))

(defun indium-debugger-locals-get-buffer ()
  "Return the buffer to use to inspect locals."
  (get-buffer (indium-debugger-locals-buffer-name)))

(defun indium-debugger-locals-buffer-name ()
  "Return the name of the buffer to use to inspect locals."
  "*JS Debugger Locals*")

(defun indium-debugger-locals-get-buffer-create ()
  "Create a locals buffer unless one exists, and return it."
  (let ((buf (indium-debugger-locals-get-buffer)))
    (unless buf
      (setq buf (generate-new-buffer (indium-debugger-locals-buffer-name)))
      (indium-debugger-locals-setup-buffer buf))
    buf))

(defun indium-debugger-locals-setup-buffer (buffer)
  "Setup BUFFER."
  (with-current-buffer buffer
    (indium-debugger-locals-mode)
    (read-only-mode)))

(defvar indium-debugger-locals-mode-map
  (let ((map (copy-keymap indium-inspector-mode-map)))
    (define-key map "g" nil)
    (define-key map "l" nil)
    map))

(define-derived-mode indium-debugger-locals-mode indium-inspector-mode "Locals"
  "Major mode for inspecting local variables in a scope-chain.

\\{indium-debugger-locals-mode-map}")

(provide 'indium-debugger-locals)
;;; indium-debugger-locals.el ends here
