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

;; Add and remove breakpoints to a buffer.
;;
;; Breakpoints are added even if Jade is not connected.  In such case, Jade will
;; attempt to put all breakpoints when a connection is made.

;; Add or remove breakpoints from buffers.

;;; Code:

(require 'jade-backend)
(require 'jade-faces)

(defun jade-breakpoint-add (&optional condition)
  "Add a breakpoint at point.

When CONDITION is non-nil, the breakpoint will be hit when
CONDITION is true."
  (let ((ov (jade-breakpoint--put-icon condition)))
    (when jade-connection
      (jade-backend-add-breakpoint (jade-backend)
                                   buffer-file-name
                                   (1- (line-number-at-pos))
                                   (lambda (line id condition)
                                     (jade-breakpoint-added id ov))
                                   condition))))

(defun jade-breakpoint-remove ()
  "Remove the breakpoint from the current line."
  (if-let ((id (jade-breakpoint-id-at-point)))
      (when jade-connection
        (jade-backend-remove-breakpoint (jade-backend) id)))
  (jade-breakpoint--remove-icon))

(defun jade-breakpoint-remove-all ()
  "Remove all breakpoints from the current buffer's file."
  (jade-breakpoint-remove-breakpoints-from-buffer)
  (when jade-connection
    (seq-do (lambda (brk)
              (jade-backend-remove-breakpoint (jade-backend)
                                              (map-elt brk 'id)))
            (jade-backend-get-breakpoints-in-file buffer-file-name))))

(defun jade-breakpoint-add-breakpoints-to-buffer ()
  "Add all breakpoints markers to the current buffer.
This function does not add breakpoints."
  (seq-do (lambda (brk)
            (save-excursion
              (goto-line (1+ (map-elt brk 'line)))
              (let ((ov (jade-breakpoint--put-icon)))
                (jade-breakpoint-added (map-elt brk 'id) ov))))
          (jade-backend-get-breakpoints-in-file buffer-file-name)))

(defun jade-breakpoint-remove-breakpoints-from-buffer ()
  "Remove all breakpoint markers from the current buffer.
This function does no unset breakpoints,"
  (remove-overlays (point-min)
                   (point-max)
                   'jade-breakpoint
                   t))

(defun jade-breakpoint-added (id overlay)
  "Add the breakpoint ID to OVERLAY."
  (jade-breakpoint--put-id id overlay))

(defun jade-breakpoint-restore-breakpoints ()
  "Restore BREAKPOINTS set to all buffers.
This function is used when reconnecting to a new connection."
  (seq-doseq (buf (buffer-list))
    (with-current-buffer buf
      (save-excursion
        (let ((overlays (overlays-in (point-min) (point-max))))
          (seq-doseq (ov overlays)
            (when (overlay-get ov 'jade-breakpoint)
              (let ((condition (overlay-get ov 'jade-condition))
                    (start (overlay-start ov))
                    (end (overlay-end ov)))
                (goto-char (overlay-start ov))
                (jade-breakpoint-add)))))))))

(defun jade-breakpoint--put-icon (&optional condition)
  "Add a breakpoint icon on the current line.
The icon is added to the left fringe.

When CONDITION is non-nil, add it to the breakpoint overlay.
Return the overlay."
  (let ((ov (make-overlay (point-at-bol) (point-at-eol))))
    (overlay-put ov
                 'before-string
                 (jade-breakpoint--fringe-icon))
    (overlay-put ov
                 'jade-breakpoint
                 t)
    (when condition
      (overlay-put ov
                   'jade-breakpoint-condition
                   condition))
    ov))

(defun jade-breakpoint--put-id (id overlay)
  "Put the ID of the breakpoint to OVERLAY."
  (overlay-put overlay
               'jade-breakpoint-id
               id))

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

(defun jade-breakpoint-id-at-point ()
  "Return the id of the breakpoint on the current line.
If there is no breakpoint set on the line, return nil."
  (seq-some (lambda (ov)
              (overlay-get ov 'jade-breakpoint-id))
            (overlays-at (point))))

(defun jade-breakpoint-on-current-line-p ()
  "Return non-nil if there is a breakpoint on the current line."
  (seq-some (lambda (ov)
              (overlay-get ov 'jade-breakpoint))
            (overlays-at (point))))

(and (display-images-p)
     (define-fringe-bitmap 'jade-breakpoint
       "\x3c\x7e\xff\xff\xff\xff\x7e\x3c"))

(provide 'jade-breakpoint)
;;; jade-breakpoint.el ends here
