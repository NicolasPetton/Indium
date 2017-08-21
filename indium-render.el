;;; indium-render.el --- Helper functions to display JS objects in buffers  -*- lexical-binding: t; -*-

;; Copyright (C) 2016-2017  Nicolas Petton

;; Author: Nicolas Petton <nicolas@petton.fr>
;; Keywords:

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

;; Helper functions for inserting content in indium buffers.

;;; Code:

(require 'indium-faces)
(require 'seq)
(require 'indium-seq-fix)
(require 'map)

(declare-function indium-backend-object-reference-p "indium-backend.el")
(declare-function indium-debugger-frames-select-frame "indium-debugger.el")
(declare-function indium-inspector-inspect "indium-inspector.el")

(defun indium-render-values (values &optional separator)
  "Render VALUES separated by SEPARATOR.
If no SEPARATOR is provided, separate VALUES by a space."
  (unless separator (setq separator " "))
  (let ((length (seq-length values)))
    (seq-map-indexed (lambda (value index)
                       (indium-render-value value)
                       (unless (<= (1- length) index)
                         (insert separator)))
                     values)))

(defun indium-render-value (value)
  "Render VALUE, based on its object type.
If VALUE represents a reference to a remote object, render it
with a link to an inspector on that object."
  (if (indium-backend-object-reference-p value)
      (indium-render-object-link value)
    (indium-render-description value)))

(defun indium-render-value-to-string (value)
  "Return a string representation of VALUE."
  (with-temp-buffer
    (indium-render-value value)
    (buffer-string)))

(defun indium-render-description (value)
  "Insert VALUE fontified as a description."
  (let ((description (indium-description-string value)))
    (insert
     (propertize description
                 'font-lock-face 'indium-repl-stdout-face
                 'rear-nonsticky '(font-lock-face)))))

(defun indium-render-keyword (string)
  "Insert STRING as fontified as a keyword."
  (insert
   (propertize string
               'font-lock-face 'indium-keyword-face
               'rear-nonsticky '(font-lock-face))))


(defun indium-render-button (string action)
  "Render a button with the label STRING.
When clicked, evaluate ACTION.
ACTION should be a function that takes no argument."
  (insert
   (propertize string
               'font-lock-face 'indium-button-face
               'indium-action action
               'rear-nonsticky '(font-lock-face indium-action))))

(defun indium-render-header (string)
  "Render STRING as a header."
  (insert
   (propertize string
               'font-lock-face 'indium-header-face
               'rear-nonsticky '(font-lock-face))))

(defun indium-render-frame (frame url current)
  "Render the stack frame FRAME with the URL of its script.
If CURRENT is non-nil, FRAME rendered as the current frame.  When
clicked, jump in the debugger to the frame."
  (insert (if current "* " "  "))
  (insert (propertize (indium-render--frame-label frame)
                      'font-lock-face (if current
                                          'indium-highlight-face
                                        'indium-link-face)
                      'rear-nonsticky '(font-lock-face indium-action)
                      'indium-action (lambda (&rest _)
                                     (indium-debugger-frames-select-frame frame))))
  (when url
    (insert (propertize (format " <%s>" url)
                        'font-lock-face 'indium-frame-url-face))))

(defun indium-description-string (value &optional full)
  "Return a short string describing VALUE.

When FULL is non-nil, do not strip long descriptions and function
definitions."
  (let ((description (map-elt value 'description))
        (type (map-elt value 'type)))
    ;; Showing the source code of the function is too verbose
    (if (and (not full) (eq type 'function))
        "function"
      description)))

(defun indium-render-object-link (value)
  "Render VALUE as a link, with an optional preview."
  (let* ((description (indium-description-string value))
         (preview (map-elt value 'preview))
         (beg (point))
         (end (progn
                (insert (indium-render--truncate-string-to-newline description))
                (point)))
         (face 'indium-link-face))
    (set-text-properties beg end
                         `(font-lock-face ,face
                                          mouse-face highlight
                                          indium-reference ,value))
    (when preview
      (insert (format " %s" preview)))))

(defun indium-render-properties (properties)
  "Insert all items in PROPERTIES sorted by name."
  (seq-map #'indium-render-property
           (seq-sort (lambda (p1 p2)
                       (string< (map-elt p1 'name)
                                (map-elt p2 'name)))
                     properties)))

(defun indium-render-property (property &optional separator)
  "Insert the remote reference PROPERTY as a value.
When SEPARATOR is non-nil, insert it after the property.
Otherwise, insert a newline."
  (insert "  " (map-elt property 'name) ": ")
  (indium-render-value (map-elt property 'value))
  (insert (or separator "\n")))

(defun indium-render-property-to-string (property)
  "Return PROPERTY rendered as a string."
  (with-temp-buffer
    (indium-render-property property "")
    (buffer-string)))

(declare #'indium-inspector-inspect)

(defun indium-follow-link ()
  "Follow the link at point."
  (interactive)
  (let ((reference (get-text-property (point) 'indium-reference))
        (action (get-text-property (point) 'indium-action)))
    (cond
     (reference (indium-inspector-inspect reference))
     (action (funcall action)))))

(defun indium-perform-action ()
  "Evaluate the button action at point."
  (let ((function (get-text-property (point) 'indium-action)))
    (funcall function)))

(defun indium-fontify-js (&rest args)
  "Fontify ARGS as JavaScript."
  (with-temp-buffer
    (js-mode)
    (apply #'insert args)
    (font-lock-fontify-region (point-min) (point-max))
    (buffer-string)))

(defun indium-message (&rest args)
  "Display ARGS like `message', but fontified as JavaScript."
  (message "%s" (apply #'indium-fontify-js args)))

(defun indium-render--truncate-string-to-newline (string)
  "Return STRING truncated before the first newline.
If STRING is truncated, append ellipsis."
  (let ((result (car (split-string string "\n"))))
    (unless (string= string result)
      (setq result (concat result "…")))
    result))

(defun indium-render--frame-label (frame)
  "Return the label for FRAME to be used in the debugger stack frame list."
  (let ((label (indium-frame-function-name frame)))
    (if (seq-empty-p label)
        (or (indium-frame-type frame) "Closure")
      label)))

(provide 'indium-render)
;;; indium-render.el ends here
