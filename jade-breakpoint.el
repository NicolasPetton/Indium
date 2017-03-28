;;; jade-breakpoint.el --- Add/remove breakpoints    -*- lexical-binding: t; -*-

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

;; Add or remove breakpoints from buffers.

;;; Code:

(require 'jade-backend)
(require 'jade-faces)

(defun jade-breakpoint-add (&optional condition)
  "Add a breakpoint at point."
  (if-let ((url (jade-workspace-make-url buffer-file-name jade-connection)))
      (jade-backend-add-breakpoint (jade-backend)
                                   url
                                   (1- (line-number-at-pos))
                                   (apply-partially #'jade-breakpoint-added
                                                    (current-buffer))
                                   condition)
    (user-error "No URL for the current buffer.  Setup Jade workspaces first")))

(defun jade-breakpoint-remove ()
  "Remove the breakpoint from the current line."
  (if-let ((id (jade-breakpoint-at-point)))
      (progn
        (jade-backend-remove-breakpoint (jade-backend) id)
        (jade-breakpoint--remove-icon))))

(defun jade-breakpoint-added (buffer id line)
  "Add a breakpoint marker to BUFFER with ID.
The marker is set at LINE."
  (save-excursion
   (switch-to-buffer buffer)
   (goto-line (1+ line))
   (jade-breakpoint--put-icon id)))

(defun jade-breakpoint--put-icon (id)
  "Add a breakpoint icon on the current line with id ID.
The icon is added to the left fringe."
  (let ((ov (make-overlay (point-at-bol) (point-at-eol))))
    (overlay-put ov
                 'before-string
                 (jade-breakpoint--fringe-icon))
    (overlay-put ov
                 'jade-breakpoint
                 t)
    (overlay-put ov
                 'jade-breakpoint-id
                 id)))

(defun jade-breakpoint--remove-icon ()
  "Remove the breakpoint icon from the current line."
  (remove-overlays (point-at-bol)
                   (point-at-eol)
                   'jade-breakpoint
                   t))

(defun jade-breakpoint--fringe-icon ()
  "Return the fringe icon used for breakpoints."
  (propertize "b" 'display
              (list 'left-fringe 'jade-breakpoint 'jade-breakpoint-face)))

(defun jade-breakpoint-at-point ()
  "Return the id of the breakpoint on the current line.
If there is no breakpoint set on the line, return nil."
  (seq-some (lambda (ov)
              (overlay-get ov 'jade-breakpoint-id))
            (overlays-at (point))))

(define-fringe-bitmap 'jade-breakpoint
  "\x3c\x7e\xff\xff\xff\xff\x7e\x3c")

(provide 'jade-breakpoint)
;;; jade-breakpoint.el ends here
