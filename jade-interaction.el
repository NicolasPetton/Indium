;;; jade-interaction.el --- Interaction functions for jade.el  -*- lexical-binding: t; -*-

;; Copyright (C) 2016  Nicolas Petton

;; Author: Nicolas Petton <nicolas@petton.fr>
;; Keywords: javascript, tools

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

;;; Code:

(require 'js2-mode)
(require 'jade-backend)
(require 'jade-inspector)
(require 'jade-render)

(defun jade-interaction-eval-last-node (arg)
  "Evaluate the node before point; print in the echo area.
This is similar to `eval-last-sexp', but for JavaScript buffers.

Interactively, with a prefix argument ARG, print output into
current buffer."
  (interactive "P")
  (jade-backend-evaluate jade-backend
                         (js2-node-string (jade-interaction-node-before-point))
                         (lambda (value _error)
                           (let ((description (jade-description-string value)))
                             (if arg
                                 (save-excursion
                                   (insert description))
                               (message description))))))

(defun jade-interaction-inspect-last-node ()
  "Evaluate and inspect the node before point."
  (interactive)
  (jade-backend-evaluate jade-backend
                         (js2-node-string (jade-interaction-node-before-point))
                         (lambda (result error)
                           (jade-inspector-inspect result))))

(defun jade-interaction-node-before-point ()
  "Return the node before point to be evaluated."
  (save-excursion
    (forward-comment -1)
    (while (looking-back "[:,]")
      (backward-char 1))
    (backward-char 1)
    (let* ((node (js2-node-at-point))
           (parent (js2-node-parent node)))
      ;; Heuristics for finding the node to evaluate: if the parent of the node
      ;; before point is a prop-get node (i.e. foo.bar) and if it starts before
      ;; the current node, meaning that the point is on the node following the
      ;; parent, then evaluate the content of the parent node:
      ;;
      ;; (underscore represents the point)
      ;; foo.ba_r // => evaluate foo.bar
      ;; foo_.bar // => evaluate foo
      ;; foo.bar.baz_() // => evaluate foo.bar.baz
      ;; foo.bar.baz()_ // => evaluate foo.bar.baz()
      (while (and (js2-prop-get-node-p parent)
                  (< (js2-node-abs-pos parent)
                     (js2-node-abs-pos node)))
        (setq node parent))
      node)))

(provide 'jade-interaction)
;;; jade-interaction.el ends here
