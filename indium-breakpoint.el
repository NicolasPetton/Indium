;;; indium-breakpoint.el --- Add/remove breakpoints    -*- lexical-binding: t; -*-

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
;; Breakpoints are added even if Indium is not connected.  In such case, Indium will
;; attempt to put all breakpoints when a connection is made.

;; Add or remove breakpoints from buffers.

;;; Code:

(require 'indium-backend)
(require 'indium-faces)
(eval-and-compile
  (require 'indium-script))

(defun indium-breakpoint-add (location &optional condition)
  "Add a breakpoint at LOCATION.

When CONDITION is non-nil, the breakpoint will be hit when
CONDITION is true."
  (let ((ov (indium-breakpoint--put-icon condition)))
    (when-indium-connected
      (indium-backend-add-breakpoint (indium-current-connection-backend)
				     location
				     (lambda (id)
				       (indium-breakpoint-added id ov))
				     condition))))

(defun indium-breakpoint-edit-condition ()
  "Edit condition of breakpoint at point."
  (let* ((breakpoint (indium-backend-get-breakpoint (indium-breakpoint-id-at-point)))
         (old-condition (map-elt breakpoint 'condition))
         (new-condition (read-from-minibuffer
                         (format "Breakpoint condition (%s): " old-condition)
                         nil nil nil nil old-condition))
         (new-condition (if (string-empty-p new-condition)
                            old-condition
                          new-condition)))
    (map-put breakpoint 'condition new-condition)
    (indium-breakpoint-remove)
    (indium-breakpoint-add (indium-script-generated-location-at-point)
			   new-condition)))

(defun indium-breakpoint-remove ()
  "Remove the breakpoint from the current line."
  (if-let ((id (indium-breakpoint-id-at-point)))
      (when-indium-connected
        (indium-backend-remove-breakpoint (indium-current-connection-backend) id)))
  (indium-breakpoint--remove-icon))

(defun indium-breakpoint-remove-all ()
  "Remove all breakpoints from the current buffer's file."
  (indium-breakpoint-remove-breakpoints-from-buffer)
  (indium-backend-remove-all-breakpoints-from-buffer (current-buffer)))

(defun indium-breakpoint-add-breakpoints-to-buffer ()
  "Add all breakpoints markers to the current buffer.
This function does not add breakpoints."
  (seq-do (lambda (brk)
            (save-excursion
              (goto-line (1+ (map-elt brk 'line)))
              (let ((ov (indium-breakpoint--put-icon)))
                (indium-breakpoint-added (map-elt brk 'id) ov))))
          (indium-backend-get-breakpoints-in-file buffer-file-name)))

(defun indium-breakpoint-remove-breakpoints-from-buffer ()
  "Remove all breakpoint markers from the current buffer.
This function does no unset breakpoints,"
  (remove-overlays (point-min)
                   (point-max)
                   'indium-breakpoint
                   t))

(defun indium-breakpoint-added (id overlay)
  "Add the breakpoint ID to OVERLAY."
  (indium-breakpoint--put-id id overlay))

(defun indium-breakpoint-update-breakpoints ()
  "Update all breakpoints for the current buffer in the backend."
  (when-indium-connected
    (indium-backend-remove-all-breakpoints-from-buffer (current-buffer))
    (indium-breakpoint-restore-breakpoints)))

(defun indium-breakpoint-restore-breakpoints ()
  "Restore all breakpoints set to all buffers.
This function is used when reconnecting to a new connection."
  (seq-doseq (buf (buffer-list))
    (with-current-buffer buf
      (save-excursion
        (let ((overlays (overlays-in (point-min) (point-max))))
          (seq-doseq (ov overlays)
            (when (overlay-get ov 'indium-breakpoint)
              (let ((condition (overlay-get ov 'indium-breakpoint-condition))
                    (start (overlay-start ov)))
                (goto-char start)
                (indium-breakpoint-add (indium-script-generated-location-at-point)
				       condition)))))))))

(defun indium-breakpoint--put-icon (&optional condition)
  "Add a breakpoint icon on the current line.
The icon is added to the left fringe.

When CONDITION is non-nil, add it to the breakpoint overlay.
Return the overlay."
  (let ((ov (indium-breakpoint-ensure-overlay)))
    (overlay-put ov
                 'before-string
                 (indium-breakpoint--fringe-icon))
    (when condition
      (overlay-put ov
                   'indium-breakpoint-condition
                   condition))
    ov))

(defun indium-breakpoint--put-id (id overlay)
  "Put the ID of the breakpoint to OVERLAY."
  (overlay-put overlay
               'indium-breakpoint-id
               id))

(defun indium-breakpoint--remove-icon ()
  "Remove the breakpoint icon from the current line."
  (remove-overlays (point-at-bol)
                   (point-at-eol)
                   'indium-breakpoint
                   t))

(defun indium-breakpoint--fringe-icon ()
  "Return the fringe icon used for breakpoints."
  (propertize "b" 'display
              (list 'left-fringe 'indium-breakpoint 'indium-breakpoint-face)))

(defun indium-breakpoint-overlay-on-current-line ()
  "Return the breakpoint overlay on the current-line.
If no overlay is present, return nil."
  (seq-find (lambda (ov)
              (overlay-get ov 'indium-breakpoint))
            (overlays-in (point-at-bol) (point-at-eol))))

(defun indium-breakpoint-ensure-overlay ()
  "Return the breakpoint overlay on the current line.
If there is no overlay, make one."
  (or (indium-breakpoint-overlay-on-current-line)
      (let ((ov (make-overlay (point-at-bol) (point-at-eol))))
        (overlay-put ov 'indium-breakpoint t)
        ov)))

(defun indium-breakpoint-id-at-point ()
  "Return the id of the breakpoint on the current line.
If there is no breakpoint set on the line, return nil."
  (when-let ((ov (indium-breakpoint-overlay-on-current-line)))
    (overlay-get ov 'indium-breakpoint-id)))

(defun indium-breakpoint-on-current-line-p ()
  "Return non-nil if there is a breakpoint on the current line."
  (not (null (indium-breakpoint-overlay-on-current-line))))

(and (display-images-p)
     (define-fringe-bitmap 'indium-breakpoint
       "\x3c\x7e\xff\xff\xff\xff\x7e\x3c"))

(provide 'indium-breakpoint)
;;; indium-breakpoint.el ends here
