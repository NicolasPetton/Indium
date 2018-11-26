;;; indium-inspector.el --- Inspector for JavaScript objects  -*- lexical-binding: t; -*-

;; Copyright (C) 2016-2018  Nicolas Petton

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
(require 'subr-x)
(require 'indium-structs)
(require 'indium-render)
(require 'indium-faces)

(declare-function indium-client-get-properties "indium-client.el")

(defvar indium-inspector-history nil)
(make-variable-buffer-local 'indium-inspector-history)

(defun indium-inspector-inspect (obj)
  "Open an inspector on the remote object OBJ."
  (if (indium-remote-object-reference-p obj)
      (indium-client-get-properties
       (indium-remote-object-id obj)
       (lambda (properties)
	 (indium-inspector--inspect-properties properties obj)))
    (message "Cannot inspect %S" (indium-remote-object-description obj))))

(defun indium-inspector--inspect-properties (properties obj)
  "Insert all PROPERTIES for the remote object OBJ."
  (let ((buf (indium-inspector-get-buffer-create))
        (inhibit-read-only t))
    (with-current-buffer buf
      (indium-inspector-push-to-history obj)
      (save-excursion
        (erase-buffer)
        (indium-render-keyword (indium-remote-object-to-string obj t))
        (insert "\n\n")
        (indium-inspector--insert-sorted-properties properties)))
    (pop-to-buffer buf)))

(defun indium-inspector--insert-sorted-properties (properties)
  "Insert sorted PROPERTIES."
  (let ((sorted-properties (indium-inspector--split-properties properties)))
          (indium-render-properties (cadr sorted-properties))
          (insert "\n")
          (when-let (native (car sorted-properties))
            (indium-render-properties native)
            (insert "\n"))))

(defun indium-inspector--split-properties (properties)
  "Split PROPERTIES into list where the first element is native properties and the second is the rest."
  (let ((split (seq-reduce (lambda (result property)
                       (push property
			     (if (indium-property-native-p property)
				 (car result)
                               (cadr result)))
                       result)
		     properties
		     (list nil nil))))
    (seq-map (lambda (list) (nreverse list)) split)))

(defun indium-inspector-pop ()
  "Go back in the history to the last object inspected."
  (interactive)
  (if (cdr indium-inspector-history)
      (progn
        (pop indium-inspector-history)
        (funcall #'indium-inspector-inspect (car indium-inspector-history)))
    (message "No previous object to inspect")))

(defun indium-inspector-goto-reference (direction)
  "Move point to the next object reference in DIRECTION.
DIRECTION can be either `next' or `previous'."
  (let* ((delta (pcase direction
                  (`next 1)
                  (`previous -1)))
         (limit-check (pcase direction
                        (`next #'eobp)
                        (`previous #'bobp)))
         (reference (save-excursion
                      (forward-line delta)
                      (when (eq direction 'previous)
                        (end-of-line))
                      (while (and (not (funcall limit-check))
                                  (not (get-text-property (point) 'indium-reference)))
                        (forward-char delta))
                      (when (get-text-property (point) 'indium-reference)
                        (point)))))
    (when reference
      (goto-char reference)
      ;; go to the first char of the reference
      (while (get-text-property (point) 'indium-reference)
        (backward-char 1))
      (forward-char 1))))

(defun indium-inspector-next-reference ()
  "Move the point to the next object reference."
  (interactive)
  (indium-inspector-goto-reference 'next))

(defun indium-inspector-previous-reference ()
  "Move the point to the previous object reference."
  (interactive)
  (indium-inspector-goto-reference 'previous))

(defun indium-inspector-refresh ()
  "Request new data to the backend and update the inspector buffer."
  (interactive)
  (when indium-inspector-history
    (funcall #'indium-inspector-inspect (car indium-inspector-history))))

(defun indium-inspector-push-to-history (reference)
  "Add REFERENCE to the inspected objects history."
  (let-alist reference
    (when (or (seq-empty-p indium-inspector-history)
	      (not (equal (indium-remote-object-id reference)
			  (indium-remote-object-id (car indium-inspector-history)))))
      (push reference indium-inspector-history))))

(defun indium-inspector-get-buffer ()
  "Return the inspector buffer, or nil if no inspector buffer exists."
  (get-buffer (indium-inspector-buffer-name)))

(defun indium-inspector-get-buffer-create ()
  "Return an inspector buffer for the current connection.
If no buffer exists, create one."
  (let ((buf (indium-inspector-get-buffer)))
    (unless buf
      (setq buf (get-buffer-create (indium-inspector-buffer-name)))
      (indium-inspector-setup-buffer buf))
    buf))

(defun indium-inspector-setup-buffer (buffer)
  "Setup the inspector BUFFER."
  (with-current-buffer buffer
    (indium-inspector-mode)))

(defun indium-inspector-buffer-name ()
  "Return the inspector buffer name for the current connection."
  "*JS Inspector*")

(defvar indium-inspector-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map [return] #'indium-follow-link)
    (define-key map "\C-m" #'indium-follow-link)
    (define-key map [mouse-1] #'indium-follow-link)
    (define-key map "l" #'indium-inspector-pop)
    (define-key map "g" #'indium-inspector-refresh)
    (define-key map "n" #'indium-inspector-next-reference)
    (define-key map "p" #'indium-inspector-previous-reference)
    (define-key map [tab] #'indium-inspector-next-reference)
    (define-key map [backtab] #'indium-inspector-previous-reference)
    map))

(define-derived-mode indium-inspector-mode special-mode "Inspector"
  "Major mode for inspecting JavaScript objects.

\\{indium-inspector-mode-map}"
  (setq buffer-read-only t)
  (font-lock-ensure)
  (setq-local electric-indent-chars nil)
  (setq-local truncate-lines t))

(provide 'indium-inspector)
;;; indium-inspector.el ends here
