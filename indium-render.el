;;; indium-render.el --- Helper functions to display JS objects in buffers  -*- lexical-binding: t; -*-

;; Copyright (C) 2016-2018  Nicolas Petton

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

(require 'seq)
(require 'indium-seq-fix)

(require 'indium-faces)
(require 'indium-structs)

(declare-function indium-debugger-frames-select-frame "indium-debugger.el")
(declare-function indium-inspector-inspect "indium-inspector.el")

(defun indium-render-remote-object (obj)
  "Render OBJ, based on its object type.
If OBJ represents a reference to an object, render it with a link
to an inspector on that object."
  (cond
   ((indium-remote-object-error-p obj)
    (indium-render-description obj 'indium-repl-error-face))
   ((indium-remote-object-reference-p obj)
    (indium-render-object-link obj))
   (t
    (indium-render-description obj 'indium-repl-stdout-face))))

(defun indium-render-remote-object-to-string (obj)
  "Return a string representation of OBJ."
  (with-temp-buffer
    (indium-render-remote-object obj)
    (buffer-string)))

(defun indium-render-description (obj face)
  "Insert OBJ fontified with FACE as a description."
  (let ((description (indium-remote-object-to-string obj)))
    (insert
     (propertize description
                 'font-lock-face face
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

(defun indium-render-object-link (obj)
  "Render OBJ as a link, with an optional preview."
  (let* ((description (indium-remote-object-to-string obj))
         (beg (point))
         (end (progn
                (insert (indium-render--truncate-string-to-newline description))
                (point)))
         (face 'indium-link-face))
    (set-text-properties beg end
                         `(font-lock-face ,face
                                          mouse-face highlight
                                          indium-reference ,obj))
    (when (indium-remote-object-has-preview-p obj)
      (insert (format " %s" (indium-remote-object-preview obj))))))

(defun indium-render-properties (properties)
  "Insert all items in PROPERTIES."
  (seq-map #'indium-render-property properties))

(defun indium-render-property (property &optional separator)
  "Insert the PROPERTY rendered as a remote object.
When SEPARATOR is non-nil, insert it after the property.
Otherwise, insert a newline."
  (insert "  " (indium-property-name property) ": ")
  (indium-render-remote-object (indium-property-remote-object property))
  (insert (or separator "\n")))

(defun indium-render-property-to-string (property)
  "Return PROPERTY rendered as a string."
  (with-temp-buffer
    (indium-render-property property "")
    (buffer-string)))

(defun indium-render-frame (frame current)
  "Render the stack frame FRAME.
If CURRENT is non-nil, FRAME rendered as the current frame.  When
clicked, jump in the debugger to the frame."
  (let ((file (indium-location-file (indium-frame-location frame))))
    (insert (if current "* " "  "))
    (insert (propertize (indium-render--frame-label frame)
			'font-lock-face (if current
                                            'indium-highlight-face
                                          'indium-link-face)
			'rear-nonsticky '(font-lock-face indium-action)
			'indium-action (lambda (&rest _)
					 (indium-debugger-frames-select-frame frame))))
    (when (not (string-empty-p file))
      (insert (propertize (format " <%s>" file)
                          'font-lock-face 'indium-frame-url-face)))))

(defun indium-render--frame-label (frame)
  "Return the label for FRAME to be used in the debugger stack frame list."
  (let ((label (indium-frame-function-name frame)))
    (if (seq-empty-p label)
        "Closure"
      label)))

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

(provide 'indium-render)
;;; indium-render.el ends here
