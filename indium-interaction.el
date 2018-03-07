;;; indium-interaction.el --- Interaction functions for indium.el  -*- lexical-binding: t; -*-

;; Copyright (C) 2016-2018  Nicolas Petton

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

(declare-function indium-backend-activate-breakpoints "indium-backend.el")
(declare-function indium-backend-deactivate-breakpoints "indium-backend.el")
(declare-function indium-workspace-make-url "indium-workspace.el")

(defvar indium-update-script-source-hook nil
  "Hook run when script source is updated.")

(defun indium-eval (string &optional callback)
  "Evaluate STRING on the current backend.
When CALLBACK is non-nil, evaluate CALLBACK with the result.

When called interactively, prompt the user for the string to be
evaluated."
  (interactive "sEvaluate JavaScript: ")
  (indium-backend-evaluate (indium-current-connection-backend) string callback))

(defun indium-eval-buffer ()
  "Evaluate the accessible portion of current buffer."
  (interactive)
  (indium-interaction--ensure-connection)
  (indium-eval (buffer-string)))

(defun indium-eval-region (start end)
  "Evaluate the region between START and END."
  (interactive "r")
  (indium-eval (buffer-substring-no-properties start end)))

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

(defun indium-switch-to-debugger ()
  "Switch to the buffer containing the Indium debugger.
The point is moved to the top stack frame.

If there is no debugging session, signal an error."
  (interactive)
  (unless (indium-current-connection-frames)
    (user-error "No debugger to switch to"))
  (indium-debugger-select-frame (seq-elt (indium-current-connection-frames) 0)))

(defvar indium-interaction-eval-node-hook nil
  "Hooks to run after evaluating node before the point.")
(add-hook 'indium-interaction-eval-node-hook #'indium-message)

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
                        (run-hook-with-args 'indium-interaction-eval-node-hook description))))))))

(defun indium-reload ()
  "Reload the page."
  (interactive)
  (indium-interaction--ensure-connection)
  (indium-backend-evaluate (indium-current-connection-backend) "window.location.reload()"))

(defun indium-inspect-last-node ()
  "Evaluate and inspect the node before point."
  (interactive)
  (indium-interaction--ensure-connection)
  (js2-mode-wait-for-parse
   (lambda ()
     (indium-inspect-expression
      (js2-node-string (indium-interaction-node-before-point))))))

(defun indium-inspect-expression (expression)
  "Prompt for EXPRESSION to be inspected."
  (interactive "sInspect expression: ")
  (indium-interaction--ensure-connection)
  (indium-eval expression
	       (lambda (result _error)
		 (indium-inspector-inspect result))))

(defun indium-switch-to-repl-buffer ()
  "Switch to the repl buffer if any."
  (interactive)
  (if-let ((buf (indium-repl-get-buffer)))
      (progn
        (setq indium-repl-switch-from-buffer (current-buffer))
        (pop-to-buffer buf t))
    (user-error "No REPL buffer open")))

(defun indium-toggle-breakpoint ()
  "Add or remove a breakpoint on current line."
  (interactive)
  (if (indium-breakpoint-on-current-line-p)
      (call-interactively #'indium-remove-breakpoint)
    (call-interactively #'indium-add-breakpoint)))

(defun indium-mouse-toggle-breakpoint (event)
  "Toggle breakpoint at mouse EVENT click point."
  (interactive "e")
  (let* ((posn (event-end event))
         (pos (posn-point posn)))
    (when (numberp pos)
      (with-current-buffer (window-buffer (posn-window posn))
        (save-excursion
          (goto-char pos)
          (call-interactively #'indium-toggle-breakpoint))))))

(defun indium-add-breakpoint (&optional condition)
  "Add a breakpoint on the current line.
If there is already a breakpoint, signal an error.

When CONDITION is non-nil, add a conditional breakpoint with
CONDITION."
  (interactive)
  (indium-interaction--guard-no-breakpoint-at-point)
  (save-excursion
    (beginning-of-line)
    (indium-breakpoint-add condition)))

(defun indium-add-conditional-breakpoint (condition)
  "Add a breakpoint with CONDITION at point.
If there is already a breakpoint, signal an error."
  (interactive "sBreakpoint condition: ")
  (indium-add-breakpoint condition))

(defun indium-edit-breakpoint-condition ()
  "Edit the condition of breakpoint at point.
Signal an error if there is no breakpoint."
  (interactive)
  (indium-interaction--guard-breakpoint-at-point)
  (indium-breakpoint-edit-condition))

(defun indium-remove-breakpoint ()
  "Remove the breakpoint at point.
If there is no breakpoint, signal an error."
  (interactive)
  (indium-interaction--guard-breakpoint-at-point)
  (indium-breakpoint-remove))

(defun indium-remove-all-breakpoints-from-buffer ()
  "Remove all breakpoints from the current buffer."
  (interactive)
  (indium-breakpoint-remove-breakpoints-from-current-buffer))

(defun indium-deactivate-breakpoints ()
  "Deactivate all breakpoints in all buffers.
Breakpoints are not removed, but the runtime won't pause when
hitting a breakpoint."
  (interactive)
  (indium-backend-deactivate-breakpoints (indium-current-connection-backend))
  (message "Breakpoints deactivated"))

(defun indium-activate-breakpoints ()
  "Activate all breakpoints in all buffers."
  (interactive)
  (indium-backend-activate-breakpoints (indium-current-connection-backend))
  (message "Breakpoints activated"))

(defun indium-list-breakpoints ()
  "List all breakpoints in the current connection."
  (interactive)
  (if-let ((xrefs (indium--make-xrefs-from-breakpoints)))
      (xref--show-xrefs xrefs nil)
    (message "No breakpoint")))

(defun indium--make-xrefs-from-breakpoints ()
  "Return a list of xref objects from all breakpoints."
  (map-apply (lambda (breakpoint buffer)
	       (let ((line (with-current-buffer buffer
			     (line-number-at-pos
			      (overlay-start
			       (indium-breakpoint-overlay breakpoint))))))
		(xref-make (indium--get-breakpoint-xref-match breakpoint buffer)
			   (xref-make-file-location (buffer-file-name buffer)
						    line
						    0))))
	     indium-breakpoint--local-breakpoints))

(defun indium--get-breakpoint-xref-match (breakpoint buffer)
  "Return the source line where BREAKPOINT is set."
  (with-current-buffer buffer
    (save-excursion
      (goto-char (point-min))
      (forward-line (1- (line-number-at-pos
			 (overlay-start
			  (indium-breakpoint-overlay breakpoint)))))
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
  (unless-indium-connected
    (user-error "No Indium connection")))

(defvar indium-interaction-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-x C-e") #'indium-eval-last-node)
    (define-key map (kbd "C-M-x") #'indium-eval-defun)
    (define-key map (kbd "C-c M-i") #'indium-inspect-last-node)
    (define-key map (kbd "C-c M-:") #'indium-inspect-expression)
    (define-key map (kbd "C-c C-z") #'indium-switch-to-repl-buffer)
    (define-key map (kbd "C-c C-k") #'indium-update-script-source)
    (define-key map [left-fringe mouse-1] #'indium-mouse-toggle-breakpoint)
    (define-key map [left-margin mouse-1] #'indium-mouse-toggle-breakpoint)
    (define-key map (kbd "C-c b t") #'indium-toggle-breakpoint)
    (define-key map (kbd "C-c b b") #'indium-add-breakpoint)
    (define-key map (kbd "C-c b c") #'indium-add-conditional-breakpoint)
    (define-key map (kbd "C-c b e") #'indium-edit-breakpoint-condition)
    (define-key map (kbd "C-c b k") #'indium-remove-breakpoint)
    (define-key map (kbd "C-c b K") #'indium-remove-all-breakpoints-from-buffer)
    (define-key map (kbd "C-c b a") #'indium-activate-breakpoints)
    (define-key map (kbd "C-c b d") #'indium-deactivate-breakpoints)
    (define-key map (kbd "C-c b l") #'indium-list-breakpoints)
    (define-key map (kbd "C-c d") #'indium-switch-to-debugger)
    (easy-menu-define indium-interaction-mode-menu map
      "Menu for Indium interaction mode"
      '("Indium interaction"
        ["Switch to REPL" indium-switch-to-repl-buffer]
        "--"
        ("Evaluation"
         ["Evaluate last node" indium-eval-last-node]
         ["Inspect last node" indium-inspect-last-node]
	 ["Inspect expression" indium-inspect-expression]
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
  "Mode for JavaScript evaluation.

\\{indium-interaction-mode-map}"
  :lighter " js-interaction"
  :keymap indium-interaction-mode-map
  (unless indium-interaction-mode
    (indium-interaction-mode-off)))

(defun indium-interaction-mode-off ()
  "Function to be evaluated when `indium-interaction-mode' is turned off."
  (indium-breakpoint-remove-overlays-from-current-buffer))

(defun indium-interaction-update ()
  "Update breakpoints and script source of the current buffer."
  (when (and indium-interaction-mode indium-current-connection)
    (indium-update-script-source)))

(defun indium-interaction-kill-buffer ()
  "Remove all breakpoints prior to killing the current buffer."
  (when indium-interaction-mode
    (indium-breakpoint-remove-breakpoints-from-current-buffer)))

(defun indium-update-script-source ()
  "Update the script source of the backend from the current buffer.
update all breakpoints set in the current buffer as well."
  (interactive)
  (when-let ((url (indium-workspace-make-url buffer-file-name)))
    (indium-backend-set-script-source
     (indium-current-connection-backend)
     url
     (buffer-string)
     (lambda ()
       (run-hook-with-args 'indium-update-script-source-hook url)))))

(defun indium-interaction--guard-breakpoint-at-point ()
  "Signal an error if there is no breakpoint on the current line."
  (unless (indium-breakpoint-at-point)
    (user-error "No breakpoint on the current line")))

(defun indium-interaction--guard-no-breakpoint-at-point ()
  "Signal an error if there is a breakpoint on the current line."
    (when (indium-breakpoint-at-point)
      (user-error "There is already a breakpoint on the current line")))

(add-hook 'after-save-hook #'indium-interaction-update)
(add-hook 'kill-buffer-hook #'indium-interaction-kill-buffer)

(provide 'indium-interaction)
;;; indium-interaction.el ends here
