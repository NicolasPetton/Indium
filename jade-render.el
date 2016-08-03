;;; jade-render.el --- Helper functions to display JS objects in buffers  -*- lexical-binding: t; -*-

;; Copyright (C) 2016  Nicolas Petton

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

;; Helper functions for inserting content in jade buffers.

;;; Code:

(require 'jade-faces)

(declare 'jade-backend-object-reference-p)

(defun jade-render-value (value &optional error)
  (if (jade-backend-object-reference-p value)
      (jade-render-object-link value error)
    (jade-render-description value error)))

(defun jade-render-description (value &optional error)
  (let ((description (jade-description-string value))
        (face (when error 'jade-repl-error-face)))
    (insert
     (propertize description
                 'font-lock-face (or face 'jade-repl-stdout-face)
                 'rear-nonsticky '(font-lock-face)))))

(defun jade-render-keyword (string)
  (insert
   (propertize string
               'font-lock-face 'jade-keyword-face
               'rear-nonsticky '(font-lock-face))))


(defun jade-render-button (string action)
  "Render a button with the label STRING.
When clicked, evaluate ACTION.
ACTION should be a function that takes no argument."
  (insert
   (propertize string
               'font-lock-face 'jade-button-face
               'jade-action action
               'reat-nons '(font-lock-face jade-action))))

(defun jade-description-string (value &optional full)
  "Return a short string describing VALUE.

When FULL is non-nil, do not strip long descriptions and function
definitions."
  (let ((description (map-elt value 'description))
        (type (map-elt value 'type)))
    ;; Showing the source code of the function is too verbose
    (if (and (not full) (eq type 'function))
        "function"
      description)))

(defun jade-render-object-link (value error)
  (let* ((description (jade-description-string value))
         (preview (map-elt value 'preview))
         (beg (point))
         (end (progn
                (insert description)
                (point)))
         (face (if error
                   'jade-repl-error-face
                 'jade-link-face)))
    (set-text-properties beg end
                         `(font-lock-face ,face
                                          mouse-face highlight
                                          jade-reference ,value))
    (when preview
      (insert preview))))

(defun jade-render-properties (properties value)
  (seq-map #'jade-render-property
           (seq-sort (lambda (p1 p2)
                       (string< (map-elt p1 'name)
                                (map-elt p2 'name)))
                     properties)))

(defun jade-render-property (property)
  (insert "  " (map-elt property 'name) ": ")
  (jade-render-value (map-elt property 'value))
  (insert "\n"))

(declare #'jade-inspector-inspect)

(defun jade-follow-link ()
  "Follow the link at point."
  (interactive)
  (let ((reference (get-text-property (point) 'jade-reference))
        (action (get-text-property (point) 'jade-action)))
    (cond
     (reference (jade-inspector-inspect reference))
     (action (funcall action)))))

(defun jade-perform-action ()
  "Evaluate the button action at point."
  (let ((function (get-text-property (point) 'jade-action)))
   (funcall function)))

(provide 'jade-render)
;;; jade-render.el ends here
