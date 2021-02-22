;;; indium-debugger-litable.el --- Display local values in debugger buffers  -*- lexical-binding: t; -*-

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

(require 'js2-mode)
(require 'js2-refactor)
(require 'subr-x)
(require 'seq)

(require 'indium-render)

(declare-function indium-debugger-get-current-scopes "indium-debugger.el" ())
(declare-function indium-debugger-get-scopes-properties "indium-debugger.el" (scope callback))
(declare-function indium-debugger-get-buffer-create "indium-debugger.el" ())

(eval-when-compile
  (when (version< emacs-version "27.1")
    (defalias 'seq-contains-p 'seq-contains)))

(defun indium-debugger-litable-setup-buffer ()
  "Render locals in the current buffer."
  (indium-debugger-get-scopes-properties
   (indium-debugger-get-current-scopes)
   (lambda (properties _)
     ;; This is just cosmetic, don't break the session
     (ignore-errors
       (with-current-buffer (indium-debugger-get-buffer-create)
	 (js2-mode-wait-for-parse
	  (lambda ()
	    (with-current-buffer (indium-debugger-get-buffer-create)
              (js2-visit-ast (indium-debugger-litable--scope-node)
			     (indium-debugger-litable-make-visitor properties))))))))))

(defun indium-debugger-litable--scope-node ()
  "Return the scope node from point."
  (or (js2r--closest #'js2-function-node-p)
       js2-mode-ast))

(defun indium-debugger-litable-unset-buffer ()
  "Remove locals from the current buffer."
  (remove-overlays (point-min)
                   (point-max)
                   'indium-litable t))

(defun indium-debugger-litable-make-visitor (properties)
  "Return an AST visitor to add overlays for values in PROPERTIES."
  (lambda (node end-p)
    (unless end-p
      (cond ((js2-function-node-p node)
             (indium-debugger-litable-visit-function-node node properties)))
      (cond ((indium-debugger-litable-local-name-node-p node)
             (indium-debugger-litable-visit-name-node node properties))))
    t))

(defun indium-debugger-litable-visit-function-node (node properties)
  "Visit the function NODE with PROPERTIES."
  (seq-do (lambda (param)
            (indium-debugger-litable-maybe-add-value-overlay param properties))
          (js2-function-node-params node)))

(defun indium-debugger-litable-visit-name-node (node properties)
  "Visit a JS2 name NODE to add an overlay displaying PROPERTIES."
  (indium-debugger-litable-maybe-add-value-overlay node properties))

(defun indium-debugger-litable-local-name-node-p (node)
  "Return non-nil if NODE represents a local variable."
  (let ((parent (js2-node-parent node)))
    (and parent (js2-name-node-p node)
         (or (js2-var-init-node-p parent)
	     (js2-object-prop-node-p parent)
             (js2-assign-node-p parent)))))

(defun indium-debugger-litable-visit-var-init-node (node properties)
  "Visit variable initialization NODE with PROPERTIES."
  (seq-do (lambda (param)
            (indium-debugger-litable-maybe-add-value-overlay param properties))
          (js2-function-node-params node)))

(defun indium-debugger-litable-maybe-add-value-overlay (node properties)
  "If NODE match PROPERTIES, add a value overlay."
  (if-let ((name (buffer-substring-no-properties (js2-node-abs-pos node)
                                                 (js2-node-abs-end node)))
           (property (seq-find (lambda (property)
                                 (string= name
					  (indium-property-name property)))
                               properties)))
      (indium-debugger-litable-add-value-overlay node property)))

(defun indium-debugger-litable-add-exception-overlay (description)
  "Add an overlay with the DESCRIPTION of an exception where an error occurs."
  (let* ((inhibit-read-only t)
         (ov (make-overlay (point-at-bol) (point-at-eol)))
         (contents (indium-debugger-litable--overlay-string
                    (format " %s" (car (split-string description "\n"))))))
    (overlay-put ov 'indium-litable t)
    (overlay-put ov 'indium-exception-description t)
    (font-lock-prepend-text-property 1
                                     (seq-length contents)
                                     'face 'font-lock-warning-face
                                     contents)
    (overlay-put ov 'after-string contents)))

(defun indium-debugger-litable-add-value-overlay (node property)
  "Add an overlay displaying the value of NODE for PROPERTY.
Ignore if the object name of NODE is not in the current scope."
  (save-excursion
    (goto-char (js2-node-abs-pos node))
    (let ((inhibit-read-only t)
          (ov (indium-debugger-litable--get-overlay-at-pos))
          (contents (string-trim (indium-render-property-to-string property)))
	  (name (indium-property-name property)))
      (unless (seq-contains-p (overlay-get ov 'indium-properties) name)
        ;; The overlay is already used to display exception details, so do not
        ;; append anything to it.
        (unless (overlay-get ov 'indium-exception-description)
          (if-let ((existing-contents (overlay-get ov 'after-string)))
              (setq contents (concat existing-contents ", " contents))
            (setq contents (concat " " contents)))
          (setq contents (indium-debugger-litable--overlay-string contents))
          (font-lock-prepend-text-property 0
                                           (seq-length contents)
                                           'face
                                           'indium-litable-face
                                           contents)
          (overlay-put ov
                       'indium-properties
                       (cons name (overlay-get ov 'indium-properties)))
          (overlay-put ov
                       'after-string
                       contents))))))

(defun indium-debugger-litable--overlay-string (string)
  "Return the STRING to be added to an overlay at the end of the line.
If the display string overflows, trim it to avoid truncating the line."
  (save-excursion
    (goto-char (point-at-eol))
    (if (>= (+ (seq-length string) (current-column)) (window-width))
        (let* ((line-number-width (if (fboundp 'line-number-display-width)
				      (line-number-display-width 'columns)
				    0))
	       (width (- (window-width) (current-column) line-number-width 1)))
          (truncate-string-to-width string width 0 nil "..."))
      string)))

(defun indium-debugger-litable--get-overlay-at-pos ()
  "Return the overlay for litable at point.
If no overlay exist, create one."
  (or (seq-find (lambda (ov)
                      (overlay-get ov 'indium-litable))
                    (overlays-in (point-at-bol) (point-at-eol)))
      (let ((ov (make-overlay (point-at-bol) (point-at-eol))))
        (overlay-put ov 'indium-litable t)
        ov)))

(provide 'indium-debugger-litable)
;;; indium-debugger-litable.el ends here
