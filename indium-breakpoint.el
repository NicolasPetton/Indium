;;; indium-breakpoint.el --- Add/remove breakpoints    -*- lexical-binding: t; -*-

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

;; Add and remove breakpoints to a buffer.
;;
;; Breakpoints can be added to buffer even if Indium is not connected.  In such
;; case, Indium will attempt to register them breakpoints in the runtime when a
;; connection is made.

;;; Code:

(require 'indium-backend)
(require 'indium-faces)
(require 'indium-structs)
(eval-and-compile
  (require 'indium-script))

(defvar indium-breakpoint--local-breakpoints (make-hash-table :weakness t)
  "Table of all local breakpoints and their buffers.")

(defun indium-breakpoint-add (&optional condition)
  "Add a breakpoint at point.

When CONDITION is non-nil, the breakpoint will be hit when
CONDITION is true."
  (if-let ((location (indium-location-at-point)))
      (let* ((brk (make-indium-breakpoint :original-location location
					  :condition (or condition ""))))
	(map-put indium-breakpoint--local-breakpoints brk (current-buffer))
	(indium-breakpoint--add-overlay brk)
	(when-indium-connected
	  (indium-backend-register-breakpoint (indium-current-connection-backend)
					 brk)))
    (user-error "Cannot place a breakpoint here")))

(defun indium-breakpoint-edit-condition ()
  "Edit condition of breakpoint at point."
  (when-let ((breakpoint (indium-breakpoint-at-point)))
    (let* ((old-condition (indium-breakpoint-condition breakpoint))
	   (new-condition (read-from-minibuffer "Breakpoint condition: "
			   old-condition nil nil nil old-condition)))
      (indium-breakpoint-remove)
      (indium-breakpoint-add new-condition))))

(defun indium-breakpoint-remove ()
  "Remove the breakpoint from the current line."
  (when-let ((brk (indium-breakpoint-at-point)))
    (when-indium-connected
      (when (indium-breakpoint-resolved brk)
	(indium-backend-unregister-breakpoint (indium-current-connection-backend)
					      (indium-breakpoint-id brk))))
    (map-delete indium-breakpoint--local-breakpoints brk)
    (indium-breakpoint--remove-overlay)))

(defun indium-breakpoint-remove-breakpoints-from-current-buffer ()
  "Remove all breakpoints from the current buffer's file."
  (indium-breakpoint--breakpoints-in-buffer-do
   (lambda (_ ov)
     (save-excursion
       (goto-char (overlay-start ov))
       (indium-breakpoint-remove)))))

(defun indium-breakpoint-resolve (id script location)
  "Update the breakpoint with ID for SCRIPT at LOCATION.

This function should be called upon breakpoint resolution by the
backend, or when a breakpoint location gets updated from the
backend."
  (let ((original-location (indium-script-original-location script location))
	(brk (indium-breakpoint-breakpoint-with-id id)))
    (setf (indium-breakpoint-resolved brk) t)
    (indium-breakpoint--update-overlay brk original-location)))

(defun indium-breakpoint-breakpoint-with-id (id)
  "Return the breakpoint with ID or nil."
  (seq-find (lambda (brk)
	      (equal id (indium-breakpoint-id brk)))
	    (map-keys indium-breakpoint--local-breakpoints)))

(defun indium-breakpoint-at-point ()
  "Return the breakpoint on the current line.
If there is no breakpoint set on the line, return nil."
  (when-let ((ov (indium-breakpoint--overlay-on-current-line)))
    (overlay-get ov 'indium-breakpoint)))

(defun indium-breakpoint-on-current-line-p ()
  "Return non-nil if there is a breakpoint on the current line."
  (not (null (indium-breakpoint--overlay-on-current-line))))

(defun indium-breakpoint-remove-overlays-from-current-buffer ()
  "Remove all breakpoint markers from the current buffer.
This function does no unset breakpoints."
  (remove-overlays (point-min)
                   (point-max)
                   'indium-breakpoint-ov
                   t))


(defun indium-breakpoint--add-overlay (breakpoint)
  "Add an overlay for BREAKPOINT on the current line.
An icon is added to the left fringe."
  (let ((ov (indium-breakpoint--ensure-overlay)))
    (overlay-put ov
                 'before-string
                 (indium-breakpoint--fringe-icon breakpoint))
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
  (indium-breakpoint--breakpoints-in-buffer-do
   (lambda (brk overlay)
     (indium-backend-unregister-breakpoint
      (indium-current-connection-backend)
      (indium-breakpoint-id brk)
      (lambda ()
	(save-excursion
	  (goto-char (overlay-start overlay))
	  (indium-breakpoint-add (indium-breakpoint-condition brk))))))))

(defun indium-breakpoint--resolve-all-breakpoints ()
  "Resolve breakpoints from all buffers."
  (let ((buffers (seq-uniq (map-values indium-breakpoint--local-breakpoints))))
   (seq-doseq (buf buffers)
     (with-current-buffer buf
       (indium-breakpoint--resolve-breakpoints-in-current-buffer)))))

(defun indium-breakpoint--unregister-all-breakpoints ()
  "Remove the registration information from all breakpoints."
  (map-apply (lambda (brk _)
	       (indium-breakpoint-unregister brk)
	       (indium-breakpoint--update-overlay
		brk
		(indium-breakpoint-original-location brk)))
	     indium-breakpoint--local-breakpoints))

(defun indium-breakpoint--resolve-breakpoints-in-current-buffer ()
  "Resolve unresolved breakpoints from the current buffer."
  (indium-breakpoint--breakpoints-in-buffer-do
   (lambda (brk _)
     (when (indium-breakpoint-can-be-resolved-p brk)
       (indium-backend-register-breakpoint (indium-current-connection-backend)
					   brk)))))

(defun indium-breakpoint--fringe-icon (breakpoint)
  "Return the fringe icon used for BREAKPOINT."
  (propertize "b" 'display
              (list 'left-fringe (if (indium-breakpoint-resolved breakpoint)
				     'indium-breakpoint-resolved
				   'indium-breakpoint-unresolved)
		    'indium-breakpoint-face)))

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

(defun indium-breakpoint--update-after-script-parsed (_)
  "Attempt to resolve unresolved breakpoints."
  (indium-breakpoint--resolve-all-breakpoints))

;; Update/Restore breakpoints
(add-hook 'indium-update-script-source-hook #'indium-breakpoint--update-after-script-source-set)
(add-hook 'indium-script-parsed-hook #'indium-breakpoint--update-after-script-parsed)
(add-hook 'indium-connection-closed-hook #'indium-breakpoint--unregister-all-breakpoints)


;; Helpers
(defun indium-breakpoint--breakpoints-in-buffer-do (fn)
  "Evaluate FN on all breakpoints in the current buffer.

FN takes two arguments, the breakpoint and its associated
overlay."
  (let ((overlays (overlays-in (point-min) (point-max))))
    (seq-doseq (ov overlays)
      (when-let ((brk (overlay-get ov 'indium-breakpoint)))
	(funcall fn brk ov)))))

(when (and (fboundp 'define-fringe-bitmap) (display-images-p))
  (define-fringe-bitmap 'indium-breakpoint-resolved
    "\x3c\x7e\xff\xff\xff\xff\x7e\x3c")
  (define-fringe-bitmap 'indium-breakpoint-unresolved
  "\x3c\x42\x81\x81\x81\x81\x42\x3c"))

(provide 'indium-breakpoint)
;;; indium-breakpoint.el ends here
