;;; indium-interaction.el --- Interaction functions for indium.el  -*- lexical-binding: t; -*-

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

;;; Commentary:

;; Minor mode for interacting with a JavaScript runtime.  This mode provides
;; commands for managing breakpoints and evaluating code.

;;; Code:

(require 'js2-mode)
(require 'map)
(require 'seq)
(require 'subr-x)
(require 'xref)
(require 'easymenu)

(require 'indium-backend)
(require 'indium-inspector)
(require 'indium-breakpoint)
(require 'indium-repl)
(require 'indium-render)
(require 'indium-script)

(declare-function indium-backend-activate-breakpoints "indium-backend.el")
(declare-function indium-backend-deactivate-breakpoints "indium-backend.el")
(declare-function indium-workspace-make-url "indium-workspace.el")

(defcustom indium-update-script-on-save nil
  "When non-nil, update (hotswap) the script source with the contents of the buffer."
  :type 'boolean
  :group 'indium)

(defvar indium-update-script-source-hook nil
  "Hook run when script source is updated.")

(defun indium-eval (string &optional callback)
  "Evaluate STRING on the current backend.
When CALLBACK is non-nil, evaluate CALLBACK with the result.

When called interactively, prompt the user for the string to be
evaluated."
  (interactive "sEvaluate JavaScript: ")
  (indium-backend-evaluate (indium-backend) string callback))

(defun indium-eval-buffer ()
  "Evaluate the accessible portion of current buffer."
  (interactive)
  (indium-interaction--ensure-connection)
  (indium-eval (buffer-string)))

(defun indium-eval-last-node (arg)
  "Evaluate the node before point; print in the echo area.
This is similar to `eval-last-sexp', but for JavaScript buffers.

Interactively, with a prefix argument ARG, print the output into
the current buffer."
  (interactive "P")
  (indium-interaction--eval-node (indium-interaction-node-before-point) arg))

(defun indium-eval-defun ()
  "Evaluate the innermost function enclosing the current point."
  (interactive)
  (if-let ((node (js2-mode-function-at-point)))
      (indium-interaction--eval-node node)
    (user-error "No function at point")))

(defun indium-interaction--eval-node (node &optional print)
  "Evaluate the AST node NODE.
If PRINT is non-nil, print the output into the current buffer."
  (indium-interaction--ensure-connection)
  (js2-mode-wait-for-parse
   (lambda ()
     (indium-eval (js2-node-string node)
                  (lambda (value _error)
                    (let ((description (indium-render-value-to-string value)))
                      (if print
                          (save-excursion
                            (insert description))
                        (indium-message "%s" description))))))))

(defun indium-reload ()
  "Reload the page."
  (interactive)
  (indium-interaction--ensure-connection)
  (indium-backend-evaluate (indium-backend) "window.location.reload()"))

(defun indium-inspect-last-node ()
  "Evaluate and inspect the node before point."
  (interactive)
  (indium-interaction--ensure-connection)
  (js2-mode-wait-for-parse
   (lambda ()
     (indium-eval (js2-node-string (indium-interaction-node-before-point))
                  (lambda (result _error)
                    (indium-inspector-inspect result))))))

(defun indium-switch-to-repl-buffer ()
  "Switch to the repl buffer if any."
  (interactive)
  (if-let ((buf (indium-repl-get-buffer)))
      (progn
        (setq indium-repl-switch-from-buffer (current-buffer))
        (pop-to-buffer buf t))
    (user-error "No REPL buffer open")))

(defun indium-add-breakpoint ()
  "Add a breakpoint at point."
  (interactive)
  (if-let ((location (indium-script-generated-location-at-point)))
      (indium-breakpoint-add location)
    (user-error "Cannot place a breakpoint here")))

(defun indium-add-conditional-breakpoint ()
  "Add a conditional breakpoint at point."
  (interactive)
  (indium-breakpoint-add (read-from-minibuffer "Breakpoint condition: ")))

(defun indium-edit-breakpoint-condition ()
  "Edit the condition of breakpoint at point."
  (interactive)
  (indium-breakpoint-edit-condition))

(defun indium-remove-breakpoint ()
  "Remove the breakpoint at point."
  (interactive)
  (indium-breakpoint-remove))

(defun indium-remove-all-breakpoints-from-buffer ()
  "Remove all breakpoints from the current buffer."
  (interactive)
  (indium-breakpoint-remove-all))

(defun indium-deactivate-breakpoints ()
  "Deactivate all breakpoints in all buffers.
Breakpoints are not removed, but the runtime won't pause when
hitting a breakpoint."
  (interactive)
  (indium-backend-deactivate-breakpoints (indium-backend))
  (message "Breakpoints deactivated"))

(defun indium-activate-breakpoints ()
  "Activate all breakpoints in all buffers."
  (interactive)
  (indium-backend-activate-breakpoints (indium-backend))
  (message "Breakpoints activated"))

(defun indium-list-breakpoints ()
  "List all breakpoints in the current connection."
  (interactive)
  (xref--show-xrefs (indium--make-xrefs-from-breakpoints) nil))

(defun indium--make-xrefs-from-breakpoints ()
  "Return a list of xref objects from all breakpoints."
  (seq-map (lambda (breakpoint)
             (xref-make (indium--get-breakpoint-xref-match breakpoint)
                        (xref-make-file-location (map-elt breakpoint 'file)
                                                 (1+ (map-elt breakpoint 'line))
                                                 0)))
           (indium-backend-get-breakpoints)))

(defun indium--get-breakpoint-xref-match (breakpoint)
  "Return the source line where BREAKPOINT is set."
  (with-current-buffer (find-file-noselect (map-elt breakpoint 'file))
    (save-excursion
      (goto-char (point-min))
      (forward-line (map-elt breakpoint 'line))
      (buffer-substring (point-at-bol) (point-at-eol)))))

(defun indium-interaction-node-before-point ()
  "Return the node before point to be evaluated."
  (save-excursion
    (forward-comment -1)
    (while (looking-back "[:,]" nil)
      (backward-char 1))
    (backward-char 1)
    (while (js2-empty-expr-node-p (js2-node-at-point))
      (backward-char 1))
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
                      (not (js2-loop-node-p node))
                      (js2-block-node-p node)))
        (setq node parent))
      node)))

(defun indium-interaction--ensure-connection ()
  "Signal an error if there is no indium connection."
  (unless indium-current-connection
    (user-error "No Indium connection")))

(defvar indium-interaction-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-x C-e") #'indium-eval-last-node)
    (define-key map (kbd "C-M-x") #'indium-eval-defun)
    (define-key map (kbd "C-c M-i") #'indium-inspect-last-node)
    (define-key map (kbd "C-c C-z") #'indium-switch-to-repl-buffer)
    (define-key map (kbd "C-c C-k") #'indium-update-script-source)
    (define-key map (kbd "C-c b b") #'indium-add-breakpoint)
    (define-key map (kbd "C-c b c") #'indium-add-conditional-breakpoint)
    (define-key map (kbd "C-c b e") #'indium-edit-breakpoint-condition)
    (define-key map (kbd "C-c b k") #'indium-remove-breakpoint)
    (define-key map (kbd "C-c b K") #'indium-remove-all-breakpoints-from-buffer)
    (define-key map (kbd "C-c b a") #'indium-activate-breakpoints)
    (define-key map (kbd "C-c b d") #'indium-deactivate-breakpoints)
    (define-key map (kbd "C-c b l") #'indium-list-breakpoints)
    (easy-menu-define indium-interaction-mode-menu map
      "Menu for Indium interaction mode"
      '("Indium interaction"
        ["Switch to REPL" indium-switch-to-repl-buffer]
        "--"
        ("Evaluation"
         ["Evaluate last node" indium-eval-last-node]
         ["Inspect last node" indium-inspect-last-node]
         ["Evaluate function" indium-eval-defun])
        "--"
        ("Breakpoints"
         ["Add breakpoint" indium-add-breakpoint]
         ["Add conditional breakpoint" indium-add-conditional-breakpoint]
         ["Remove breakpoint" indium-remove-breakpoint]
         ["Remove all breakpoints" indium-remove-all-breakpoints-from-buffer]
         ["Deactivate breakpoints" indium-deactivate-breakpoints]
         ["Activate breakpoints" indium-activate-breakpoints]
         ["List all breakpoints" indium-list-breakpoints])))
    map))

(define-minor-mode indium-interaction-mode
  "Mode for JavaScript evalution.

\\{indium-interaction-mode-map}"
  :lighter " js-interaction"
  :keymap indium-interaction-mode-map
  (if indium-interaction-mode
      (indium-interaction-mode-on)
    (indium-interaction-mode-off)))

(defun indium-interaction-mode-on ()
  "Function to be evaluated when `indium-interaction-mode' is turned on."
  (when indium-current-connection
    (indium-breakpoint-add-breakpoints-to-buffer)))

(defun indium-interaction-mode-off ()
  "Function to be evaluated when `indium-interaction-mode' is turned off."
  (indium-breakpoint-remove-breakpoints-from-buffer))

(defun indium-interaction-update ()
  "Update breakpoints and script source of the current buffer."
  (when (and indium-interaction-mode indium-current-connection)
    (indium-breakpoint-update-breakpoints)
    (when indium-update-script-on-save
      (indium-update-script-source))))

(defun indium-update-script-source ()
  "Update the script source of the backend based on the current buffer."
  (interactive)
  (when-let ((url (indium-workspace-make-url buffer-file-name)))
    (indium-backend-set-script-source (indium-backend)
                                      url
                                      (buffer-string)
                                      (lambda ()
                                        (run-hook-with-args 'indium-update-script-source-hook url)))))

(add-hook 'after-save-hook #'indium-interaction-update)

(provide 'indium-interaction)
;;; indium-interaction.el ends here
