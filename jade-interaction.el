;;; jade-interaction.el --- Interaction functions for jade.el  -*- lexical-binding: t; -*-

;; Copyright (C) 2016-2017  Nicolas Petton

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
(require 'map)
(require 'seq)
(require 'subr-x)
(require 'jade-backend)
(require 'jade-inspector)
(require 'jade-breakpoint)
(require 'jade-repl)
(require 'jade-render)

(defun jade-eval (string &optional callback)
  "Evaluate STRING on the current backend.
When CALLBACK is non-nil, evaluate CALLBACK with the result.

When called interactively, prompt the user for the string to be
evaluated."
  (interactive "sEvaluate JavaScript: ")
  (jade-backend-evaluate (jade-backend) string callback))

(defun jade-eval-buffer ()
  "Evaluate the accessible portion of current buffer."
  (interactive)
  (jade-interaction--ensure-connection)
  (jade-eval (buffer-string)))

(defun jade-eval-last-node (arg)
  "Evaluate the node before point; print in the echo area.
This is similar to `eval-last-sexp', but for JavaScript buffers.

Interactively, with a prefix argument ARG, print output into
current buffer."
  (interactive "P")
  (jade-interaction--ensure-connection)
  (jade-eval (js2-node-string (jade-interaction-node-before-point))
             (lambda (value _error)
               (let ((description (jade-render-value-to-string value)))
                 (if arg
                     (save-excursion
                       (insert description))
                   (jade-message "%s" description))))))

(defun jade-reload ()
  "Reload the page."
  (interactive)
  (jade-interaction--ensure-connection)
  (jade-backend-evaluate (jade-backend) "window.location.reload()"))

(defun jade-inspect-last-node ()
  "Evaluate and inspect the node before point."
  (interactive)
  (jade-interaction--ensure-connection)
  (jade-eval (js2-node-string (jade-interaction-node-before-point))
             (lambda (result _error)
               (jade-inspector-inspect result))))

(defun jade-switch-to-repl-buffer ()
  "Switch to the repl buffer if any."
  (interactive)
  (if-let ((buf (jade-repl-get-buffer)))
      (switch-to-buffer buf)
    (user-error "No REPL buffer open")))

(defun jade-toggle-breakpoint (arg)
  "Add a breakpoint at point."
  (interactive "P")
  (if (jade-breakpoint-on-current-line-p)
      (jade-breakpoint-remove)
    (jade-breakpoint-add
     (when arg (read-from-minibuffer "Breakpoint condition: ")))))

(defun jade-remove-all-breakpoints-from-buffer ()
  "Remove all breakpoints from the current buffer."
  (interactive)
  (jade-breakpoint-remove-all))

(defun jade-interaction-node-before-point ()
  "Return the node before point to be evaluated."
  (save-excursion
    (forward-comment -1)
    (while (looking-back "[:,]" nil)
      (backward-char 1))
    (backward-char 1)
    (let* ((node (js2-node-at-point))
           (parent (js2-node-parent node)))
      ;; Heuristics for finding the node to evaluate: if the parent of the node
      ;; before point is a prop-get node (i.e. foo.bar) and if it starts before
      ;; the current node, meaning that the point is on the node following the
      ;; parent, then return the parent node:
      ;;
      ;; (underscore represents the point)
      ;; foo.ba_r // => evaluate foo.bar
      ;; foo_.bar // => evaluate foo
      ;; foo.bar.baz_() // => evaluate foo.bar.baz
      ;; foo.bar.baz()_ // => evaluate foo.bar.baz()
      ;;
      ;; If the node is a "block node" (i.e. the `{...}' part of a function
      ;; declaration, also return the parent node.
      (while (or (and (js2-prop-get-node-p parent)
                      (< (js2-node-abs-pos parent)
                         (js2-node-abs-pos node)))
                 (and (not (js2-function-node-p node))
                      (js2-block-node-p node)))
        (setq node parent))
      node)))

(defun jade-interaction--ensure-connection ()
  "Signal an error if there is no jade connection."
  (unless jade-connection
    (user-error "No Jade connection")))

(defvar jade-interaction-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-x C-e") #'jade-eval-last-node)
    (define-key map (kbd "C-c M-i") #'jade-inspect-last-node)
    (define-key map (kbd "C-c C-z") #'jade-switch-to-repl-buffer)
    (define-key map (kbd "C-c b b") #'jade-toggle-breakpoint)
    (define-key map (kbd "C-c b K") #'jade-remove-all-breakpoints-from-buffer)
    map))

;;;###autoload
(define-minor-mode jade-interaction-mode
  "Mode for JavaScript evalution.

\\{jade-interaction-mode-map}"
  :lighter " js-interaction"
  :keymap jade-interaction-mode-map
  (if jade-interaction-mode
      (jade-interaction-mode-on)
    (jade-interaction-mode-off)))

(defun jade-interaction-mode-on ()
  "Function to be evaluated when `jade-interaction-mode' is turned on."
  (when jade-connection
    (jade-breakpoint-add-breakpoints-to-buffer)))

(defun jade-interaction-mode-off ()
  "Function to be evaluated when `jade-interaction-mode' is turned off."
  (jade-breakpoint-remove-breakpoints-from-buffer))


(provide 'jade-interaction)
;;; jade-interaction.el ends here
