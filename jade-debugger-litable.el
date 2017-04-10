;;; jade-debugger-litable.el --- Display local values in debugger buffers  -*- lexical-binding: t; -*-

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

(require 'js2-mode)
(require 'subr-x)
(require 'seq)
(require 'jade-render)

(declare-function jade-debugger-get-current-scopes "jade-debugger" ())
(declare-function jade-debugger-get-scope-properties "jade-debugger" (scope callback))

(defun jade-debugger-litable-setup-buffer ()
  "Render locals in the current buffer."
  (let ((scope (car (jade-debugger-get-current-scopes))))
    (jade-debugger-get-scope-properties
     scope
     (lambda (properties _)
       ;; This is just cosmetic, don't break the session
       (ignore-errors
         (js2-mode-wait-for-parse
          (lambda ()
            (js2-visit-ast js2-mode-ast
                           (jade-debugger-litable-make-visitor properties)))))))))

(defun jade-debugger-litable-unset-buffer ()
  "Remove locals from the current buffer."
  (remove-overlays (point-min)
                   (point-max)
                   'jade-litable t))

(defun jade-debugger-litable-make-visitor (properties)
  "Return an AST visitor to add overlays for values in PROPERTIES."
  (lambda (node end-p)
    (unless end-p
      (cond ((js2-function-node-p node)
             (jade-debugger-litable-visit-function-node node properties)))
      (cond ((jade-debugger-litable-local-name-node-p node)
             (jade-debugger-litable-visit-name-node node properties))))
    t))

(defun jade-debugger-litable-visit-function-node (node properties)
  "Visit the function NODE with PROPERTIES."
  (seq-do (lambda (param)
            (jade-debugger-litable-maybe-add-value-overlay param properties))
          (js2-function-node-params node)))

(defun jade-debugger-litable-visit-name-node (node properties)
  "Visit a JS2 name NODE to add an overlay displaying PROPERTIES."
  (jade-debugger-litable-maybe-add-value-overlay node properties))

(defun jade-debugger-litable-local-name-node-p (node)
  "Return non-nil if NODE represents a local variable."
  (let ((parent (js2-node-parent node)))
    (and parent (js2-name-node-p node)
         (or (js2-var-init-node-p parent)
             (js2-assign-node-p parent)))))

(defun jade-debugger-litable-visit-var-init-node (node properties)
  "Visit variable initialization NODE with PROPERTIES."
  (seq-do (lambda (param)
            (jade-debugger-litable-maybe-add-value-overlay param properties))
          (js2-function-node-params node)))

(defun jade-debugger-litable-maybe-add-value-overlay (node properties)
  "If NODE match PROPERTIES, add a value overlay."
  (if-let ((name (buffer-substring-no-properties (js2-node-abs-pos node)
                                                 (js2-node-abs-end node)))
           (property (seq-find (lambda (property)
                                 (string= name
                                          (map-elt property 'name)))
                               properties)))
      (jade-debugger-litable-add-value-overlay node property)))

(defun jade-debugger-litable-add-value-overlay (node property)
  "Add an overlay displaying the value of NODE for PROPERTY.
Ignore if the object name of NODE is not in the current scope."
  (save-excursion
    (goto-char (js2-node-abs-pos node))
    (let ((inhibit-read-only t)
          (ov (jade-debugger-litable--get-overlay-at-pos))
          (contents (string-trim (jade-render-property-to-string property)))
          (name (map-elt property 'name)))
      (unless (seq-contains (overlay-get ov 'jade-properties) name)
        (if-let ((existing-contents (overlay-get ov 'after-string)))
            (setq contents (concat existing-contents ", " contents))
          (setq contents (concat " " contents)))
        (setq contents (jade-debugger-litable--overlay-string contents))
        (font-lock-prepend-text-property 0
                                         (seq-length contents)
                                         'face
                                         'jade-litable-face
                                         contents)
        (overlay-put ov
                     'jade-properties
                     (cons name (overlay-get ov 'jade-properties)))
        (overlay-put ov
                     'after-string
                     contents)))))

(defun jade-debugger-litable--overlay-string (string)
  "Return the STRING to be added to an overlay at the end of the line.
If the display string overflows, trim it to avoid truncating the line."
  (save-excursion
    (goto-char (point-at-eol))
    (if (>= (+ (seq-length string) (current-column)) (window-width))
        (let ((width (- (window-width) (current-column) 1)))
          (truncate-string-to-width string width 0 nil "..."))
      string)))

(defun jade-debugger-litable--get-overlay-at-pos ()
  "Return the overlay for litable at point.
If no overlay exist, create one."
  (or (seq-find (lambda (ov)
                      (overlay-get ov 'jade-litable))
                    (overlays-in (point-at-bol) (point-at-eol)))
      (let ((ov (make-overlay (point-at-bol) (point-at-eol))))
        (overlay-put ov 'jade-litable t)
        ov)))

(provide 'jade-debugger-litable)
;;; jade-debugger-litable.el ends here
