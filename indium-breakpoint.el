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
(require 'indium-structs)
(eval-and-compile
  (require 'indium-script))

(defun indium-breakpoint-add (&optional condition)
  "Add a breakpoint at point.

When CONDITION is non-nil, the breakpoint will be hit when
CONDITION is true."
  (if-let ((location (indium-script-generated-location-at-point)))
      (let* ((brk (make-indium-breakpoint :file (indium-location-file location)
					  :line (indium-location-line location)
					  :column (indium-location-column location)
					  :condition (or condition ""))))
	(indium-breakpoint--add-overlay brk)
	(when-indium-connected
	  (indium-backend-add-breakpoint (indium-current-connection-backend)
					 brk)))
    (user-error "Cannot place a breakpoint here")))

(defun indium-breakpoint-edit-condition ()
  "Edit condition of breakpoint at point."
  (when-let ((breakpoint (indium-breakpoint-at-point)))
    (let* ((old-condition (indium-breakpoint-condition breakpoint))
	   (new-condition (read-from-minibuffer "Breakpoint condition: "
			   old-condition nil nil nil old-condition)))
      (setf (indium-breakpoint-condition breakpoint) new-condition)
      (indium-breakpoint-remove)
      (indium-breakpoint-add new-condition))))

(defun indium-breakpoint-remove ()
  "Remove the breakpoint from the current line."
  (when-let ((brk (indium-breakpoint-at-point)))
    (when-indium-connected
      (indium-backend-remove-breakpoint (indium-current-connection-backend)
					(indium-breakpoint-id brk)))
    (indium-breakpoint--remove-overlay)))

(defun indium-breakpoint-remove-all ()
  "Remove all breakpoints from the current buffer's file."
  (indium-breakpoint-remove-all-overlays)
  (indium-breakpoint-remove-breakpoints-from-backend))

(defun indium-breakpoint-resolve (id script location)
  "Update the breakpoint with ID for SCRIPT at LOCATION.

This function should be called upon breakpoint resolution by the
backend, or when a breakpoint location gets updated from the
backend."
  (let ((original-location (indium-script-original-location script location))
	(brk (indium-current-connection-get-breakpoint id)))
    (indium-breakpoint--update-overlay brk original-location)))

(defun indium-breakpoint-at-point ()
  "Return the breakpoint on the current line.
If there is no breakpoint set on the line, return nil."
  (when-let ((ov (indium-breakpoint--overlay-on-current-line)))
    (overlay-get ov 'indium-breakpoint)))

(defun indium-breakpoint-on-current-line-p ()
  "Return non-nil if there is a breakpoint on the current line."
  (not (null (indium-breakpoint--overlay-on-current-line))))

(defun indium-breakpoint-add-breakpoints-to-buffer ()
  "Add all breakpoints markers to the current buffer.
This function does not register breakpoints."
  (seq-do (lambda (brk)
            (save-excursion
	      (goto-char (point-min))
	      (forward-line (indium-location-line (indium-breakpoint-location brk)))
	      (indium-breakpoint--add-overlay brk)))
          (indium-current-connection-get-breakpoints-in-file buffer-file-name)))

(defun indium-breakpoint-remove-all-overlays ()
  "Remove all breakpoint markers from the current buffer.
This function does no unset breakpoints."
  (remove-overlays (point-min)
                   (point-max)
                   'indium-breakpoint-ov
                   t))

(defun indium-breakpoint-remove-breakpoints-from-backend ()
  "Remove all breakpoints from the current buffer.
This function does not remove any breakpoint overlay."
  (seq-do (lambda (brk)
	    (indium-backend-remove-breakpoint (indium-current-connection-backend)
					      (indium-breakpoint-id brk)))
	  (indium-current-connection-get-breakpoints-in-file buffer-file-name)))


(defun indium-breakpoint--add-overlay (breakpoint)
  "Add an overlay for BREAKPOINT on the current line.
An icon is added to the left fringe."
  (let ((ov (indium-breakpoint--ensure-overlay)))
    (overlay-put ov
                 'before-string
                 (indium-breakpoint--fringe-icon))
    (overlay-put ov
		 'indium-breakpoint
		 breakpoint)
    (setf (indium-breakpoint-overlay breakpoint) ov)
    ov))

(defun indium-breakpoint--remove-overlay ()
  "Remove the breakpoint overlay from the current line."
  (let ((ov (indium-breakpoint--overlay-on-current-line)))
    (setf (indium-breakpoint-overlay (overlay-get ov 'indium-breakpoint)) nil)
    (remove-overlays (overlay-start ov)
		     (overlay-end ov)
		     'indium-breakpoint-ov
		     t)))

(defun indium-breakpoint--update-overlay (breakpoint location)
  "Set the overlay for BREAKPOINT at LOCATION."
  (when-let ((buf (indium-breakpoint-buffer breakpoint)))
    (with-current-buffer buf
      (save-excursion
	(goto-char (overlay-start (indium-breakpoint-overlay breakpoint)))
	(indium-breakpoint--remove-overlay))))
  (when-let ((file (indium-location-file location))
	     (line (indium-location-line location)))
    (with-current-buffer (find-file-noselect file)
      (save-excursion
	(goto-char (point-min))
	(forward-line line)
	(indium-breakpoint--add-overlay breakpoint)))))

(defun indium-breakpoint--update-breakpoints-in-current-buffer ()
  "Update the breakpoints for the current buffer in the backend."
  (let ((overlays (overlays-in (point-min) (point-max))))
    (seq-doseq (ov overlays)
      (when-let ((brk (overlay-get ov 'indium-breakpoint))
		 (start (overlay-start ov)))
	(indium-backend-remove-breakpoint
	 (indium-current-connection-backend)
	 (indium-breakpoint-id brk)
	 (lambda ()
	   (save-excursion
	     (goto-char start)
	     (indium-breakpoint-add (indium-breakpoint-condition brk)))))))))

(defun indium-breakpoint--resolve-breakpoints-in-all-buffers (&optional pred)
  "Resolve breakpoints from all buffers.

When PRED is non-nil, only resolve breakpoints which satisfy (PRED brk)."
  (seq-doseq (buf (buffer-list))
    (with-current-buffer buf
      (indium-breakpoint--resolve-breakpoints pred))))

(defun indium-breakpoint--resolve-breakpoints (&optional pred)
  "Resolve breakpoints from the current buffer.

When PRED is non-nil, only resolve breakpoints which
satisfy (PRED brk)."
  (save-excursion
    (let ((overlays (overlays-in (point-min) (point-max))))
      (seq-doseq (ov overlays)
	(when-let ((brk (overlay-get ov 'indium-breakpoint))
		   (start (overlay-start ov)))
	  (when (or (null pred)
		    (funcall pred brk))
	    (goto-char start)
	    (indium-breakpoint-add (indium-breakpoint-condition brk))))))))

(defun indium-breakpoint--fringe-icon ()
  "Return the fringe icon used for breakpoints."
  (propertize "b" 'display
              (list 'left-fringe 'indium-breakpoint 'indium-breakpoint-face)))

(defun indium-breakpoint--overlay-on-current-line ()
  "Return the breakpoint overlay on the current-line.
If no overlay is present, return nil."
  (seq-find (lambda (ov)
              (overlay-get ov 'indium-breakpoint-ov))
            (overlays-in (point-at-bol) (1+ (point-at-eol)))))

(defun indium-breakpoint--ensure-overlay ()
  "Return the breakpoint overlay on the current line.
If there is no overlay, make one."
  (or (indium-breakpoint--overlay-on-current-line)
      (let ((ov (make-overlay (point-at-bol) (point-at-eol) nil t)))
        (overlay-put ov 'indium-breakpoint-ov t)
        ov)))

(defun indium-breakpoint--update-after-script-source-set (&rest _)
  "Update the breakpoints in the current buffer each time its source is set."
  (indium-breakpoint--update-breakpoints-in-current-buffer))

(defun indium-breakpoint--update-after-script-parsed (script)
  "Attempt to resolve unresolved breakpoints for SCRIPT."
  (indium-breakpoint--resolve-breakpoints-in-all-buffers
   (lambda (brk)
     (and (indium-breakpoint-unresolved-p brk)
	  (eq script
	      (indium-script-find-from-file (indium-location-file
					     (indium-script-generated-location
					      (indium-breakpoint-location brk)))))))))

;; Update/Restore breakpoints
(add-hook 'indium-update-script-source-hook #'indium-breakpoint--update-after-script-source-set)
(add-hook 'indium-script-parsed-hook #'indium-breakpoint--update-after-script-parsed)

(and (display-images-p)
     (define-fringe-bitmap 'indium-breakpoint
       "\x3c\x7e\xff\xff\xff\xff\x7e\x3c"))

(provide 'indium-breakpoint)
;;; indium-breakpoint.el ends here
