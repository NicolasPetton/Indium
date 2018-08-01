;;; indium-repl.el --- JavaScript REPL connected to a browser tab  -*- lexical-binding: t; -*-

;; Copyright (C) 2016-2018  Nicolas Petton

;; Author: Nicolas Petton <nicolas@petton.fr>
;; Keywords: convenience, tools, javascript

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

;; REPL interactions with a browser connection.

;;; Code:

(require 'indium-render)
(require 'indium-faces)
(require 'indium-client)

(require 'company)
(require 'easymenu)
(require 'map)
(require 'js)

(require 'subr-x)
(require 'ansi-color)

(declare-function indium-inspector-inspect "indium-inspector.el")
(declare-function indium-maybe-quit "indium-interaction.el")

(defgroup indium-repl nil
  "Interaction with the REPL."
  :prefix "indium-repl-"
  :group 'indium)

(defvar indium-repl-evaluate-hook nil
  "Hook run when input is evaluated in the repl.")

(defvar indium-repl-switch-from-buffer nil
  "The buffer from which repl was activated last time.")

(defvar indium-repl-history nil "History of the REPL inputs.")
(make-variable-buffer-local 'indium-repl-history)
(defvar indium-repl-history-position -1 "Position in the REPL history.")
(make-variable-buffer-local 'indium-repl-history-position)

(defvar-local indium-repl-input-start-marker nil)
(defvar-local indium-repl-output-start-marker nil)
(defvar-local indium-repl-output-end-marker nil)

(defmacro indium-save-marker (marker &rest body)
  "Save MARKER and execute BODY."
  (declare (indent 1) (debug t))
  (let ((pos (make-symbol "pos")))
    `(let ((,pos (marker-position ,marker)))
       (prog1 (progn . ,body)
         (set-marker ,marker ,pos)))))

(defun indium-repl-setup ()
  "Create and switch to the REPL buffer."
  (switch-to-buffer (indium-repl-get-buffer-create)))

(defun indium-repl-get-buffer-create ()
  "Return a new REPL buffer."
  (let* ((buf (get-buffer-create (indium-repl-buffer-name))))
    (indium-repl-setup-buffer buf)
    buf))

(defun indium-repl-get-buffer ()
  "Return the REPL buffer, or nil."
  (get-buffer (indium-repl-buffer-name)))

(defun indium-repl-buffer-name ()
  "Return the name of the REPL buffer."
  "*JS REPL*")

(defun indium-repl-setup-buffer (buffer)
  "Setup the REPL BUFFER."
  (with-current-buffer buffer
    (unless (eq major-mode 'indium-repl-mode)
      (indium-repl-mode)
      (indium-repl-setup-markers)
      (indium-repl-mark-output-start)
      (indium-repl-insert-prompt)
      (indium-repl-mark-input-start)
      (indium-repl-emit-console-message
       `((result . ,(indium-remote-object-create
		     :description (indium-repl--welcome-message))))))))

(defun indium-repl--welcome-message ()
  "Return the welcome message displayed in new REPL buffers."
  (substitute-command-keys
   "/* Welcome to Indium!

Getting started:

- Press <\\[indium-repl-return]> on links to open an inspector
- Press <\\[indium-repl-previous-input]> and <\\[indium-repl-next-input]> to navigate in the history
- Use <\\[indium-scratch]> to open a scratch buffer for JS evaluation
- Press <\\[describe-mode]> to see a list of available keybindings
- Press <\\[indium-repl-clear-output]> to clear the output

To disconnect from the JavaScript process, press <\\[indium-quit]>.
Doing this will also close all inspectors and debugger buffers
connected to the process.

*/"))

(defun indium-repl-setup-markers ()
  "Setup the initial markers for the current REPL buffer."
  (dolist (marker '(indium-repl-output-start-marker
                    indium-repl-output-end-marker
                    indium-repl-input-start-marker))
    (set marker (make-marker))
    (set-marker (symbol-value marker) (point))))

(defun indium-repl-mark-output-start ()
  "Mark the output start."
  (set-marker indium-repl-output-start-marker (point))
  (set-marker indium-repl-output-end-marker (point)))

(defun indium-repl-mark-input-start ()
  "Mark the input start."
  (set-marker indium-repl-input-start-marker (point)))

(defun indium-repl-insert-prompt ()
  "Insert the prompt in the REPL buffer."
  (goto-char indium-repl-input-start-marker)
  (indium-save-marker indium-repl-output-start-marker
    (indium-save-marker indium-repl-output-end-marker
      (unless (bolp)
        (insert-before-markers "\n"))
      (insert-before-markers "js> ")
      (let ((beg (save-excursion
                   (beginning-of-line)
                   (point)))
            (end (point)))
        (set-text-properties beg end
                             '(font-lock-face indium-repl-prompt-face
                                              read-only t
                                              intangible t
                                              field indium-repl-prompt
                                              rear-nonsticky (read-only font-lock-face intangible field)))))))

(defun indium-repl-return ()
  "Depending on the position of point, jump to a reference of evaluate the input."
  (interactive)
  (cond
   ((get-text-property (point) 'indium-reference) (indium-follow-link))
   ((get-text-property (point) 'indium-action) (indium-perform-action))
   ((indium-repl--in-input-area-p) (indium-repl-evaluate (indium-repl--input-content)))
   (t (error "No input or action at point"))))

(defun indium-repl-inspect ()
  "Inspect the result of the evaluation of the input at point."
  (interactive)
  (indium-client-evaluate (indium-repl--input-content)
			   (lambda (result)
			     (indium-inspector-inspect result))))

(defun indium-repl--input-content ()
  "Return the content of the current input."
  (buffer-substring-no-properties indium-repl-input-start-marker (point-max)))

(defun indium-repl--in-input-area-p ()
  "Return t if in input area."
  (<= indium-repl-input-start-marker (point)))

(defun indium-repl-evaluate (string)
  "Evaluate STRING in the browser tab and emit the output."
  (push string indium-repl-history)
  (indium-client-evaluate string #'indium-repl-emit-value)
  ;; move the output markers so that output is put after the current prompt
  (save-excursion
    (goto-char (point-max))
    (set-marker indium-repl-output-start-marker (point))
    (set-marker indium-repl-output-end-marker (point))))

(defun indium-repl-emit-value (value)
  "Emit a string representation of the remote object VALUE."
  (with-current-buffer (indium-repl-get-buffer)
    (save-excursion
      (goto-char (point-max))
      (insert-before-markers "\n")
      (set-marker indium-repl-output-start-marker (point))
      (indium-render-remote-object value)
      (insert "\n")
      (indium-repl-mark-input-start)
      (set-marker indium-repl-output-end-marker (point)))
    (indium-repl-insert-prompt)
    (run-hooks 'indium-repl-evaluate-hook)))

(defun indium-repl-emit-console-message (message &optional error)
  "Emit a console MESSAGE.
When ERROR is non-nil, display MESSAGE as an error.

MESSAGE is a map (alist/hash-table) with the following keys:
  type		type of message
  url		url of the message origin
  line		line number in the resource that generated this message
  result 	object to be logged

MESSAGE must contain `result'.  Other fields are
optional."
  (with-current-buffer (indium-repl-get-buffer)
    (let-alist message
      (when (string= .type 'error)
	(setq error t)))
    (save-excursion
      (goto-char indium-repl-output-end-marker)
      (set-marker indium-repl-output-start-marker (point))
      (insert "\n")
      (when error
        (indium-repl--emit-logging-error))
      (indium-repl--emit-message message)
      (set-marker indium-repl-output-end-marker (point))
      (unless (eolp)
        (insert "\n")))))

(defun indium-repl--emit-message (message)
  "Emit the value of console MESSAGE."
  (let-alist message
    (indium-render-remote-object .result)
    (indium-repl--emit-message-url-line .url .line)))

(defun indium-repl--emit-logging-error ()
  "Emit a red \"Error\" label."
  (insert
   (ansi-color-apply
    (propertize "Error:"
                'font-lock-face 'indium-repl-error-face
                'rear-nonsticky '(font-lock-face)))
   " "))

(defun indium-repl--emit-message-url-line (url line)
  "Emit the URL and LINE for a message."
  (unless (seq-empty-p url)
    (insert "\nFrom "
            (propertize (if line
                            (format "%s:%s" url line)
                          url)
                        'font-lock-face 'indium-link-face
                        'indium-action (lambda ()
                                         (if (file-regular-p url)
                                             (find-file url)
                                           (browse-url url)))
                        'rear-nonsticky '(font-lock-face indium-action)))))

(defun indium-repl-next-input ()
  "Insert the content of the next input in the history."
  (interactive)
  (indium-repl--history-replace 'forward))

(defun indium-repl-previous-input ()
  "Insert the content of the previous input in the history."
  (interactive)
  (indium-repl--history-replace 'backward))

(defun indium-repl--history-replace (direction)
  "Replace the current input with one the next one in DIRECTION.
DIRECTION is `forward' or `backard' (in the history list)."
  (let* ((history (seq-reverse indium-repl-history))
         (search-in-progress (or (eq last-command 'indium-repl-previous-input)
                                 (eq last-command 'indium-repl-next-input)))
         (step (pcase direction
                 (`forward 1)
                 (`backward -1)))
         (pos (or (and search-in-progress (+ indium-repl-history-position step))
                  (1- (seq-length history)))))
    (unless (>= pos 0)
      (user-error "Beginning of history"))
    (unless (< pos (seq-length history))
      (user-error "End of history"))
    (setq indium-repl-history-position pos)
    (indium-repl--replace-input (seq-elt history pos))))

(defun indium-repl--replace-input (input)
  "Replace the current input with INPUT."
  (goto-char (point-max))
  (delete-region indium-repl-input-start-marker (point))
  (insert input))

(defun indium-repl-clear-output ()
  "Clear all output contents of the current buffer."
  (interactive)
  (let ((inhibit-read-only t))
    (save-excursion
      (goto-char (point-min))
      (delete-region (point) indium-repl-output-end-marker))))

(defun indium-repl-pop-buffer ()
  "Switch to the buffer from which repl was opened buffer if any."
  (interactive)
  (when indium-repl-switch-from-buffer
    (pop-to-buffer indium-repl-switch-from-buffer t)))

(defun company-indium-repl (command &optional arg &rest _args)
  "Indium REPL backend for company-mode.
See `company-backends' for more info about COMMAND and ARG."
  (interactive (list 'interactive))
  (cl-case command
    (interactive (company-begin-backend 'company-indium-repl))
    (prefix (indium-repl-company-prefix))
    (ignore-case t)
    (sorted t)
    (candidates (cons :async
                      (lambda (callback)
                        (indium-repl-get-completions arg callback))))))

(defun indium-repl-get-completions (prefix callback)
  "Get the completion list matching PREFIX.
Evaluate CALLBACK with the completion candidates."
  (let* ((input (buffer-substring-no-properties
                 (let ((bol (point-at-bol))
                       (prev-delimiter (1+ (save-excursion
                                             (re-search-backward "[([:space:]]" nil t)))))
                   (if prev-delimiter
                       (max bol prev-delimiter)
                     bol))
                 (point)))
	 (expression  (if (string-match-p "\\." input)
			  (replace-regexp-in-string "\\.[^\\.]*$" "" input)
			"this")))
    (indium-client-get-completion
     expression
     (lambda (candidates)
       (funcall callback
		(seq-filter (lambda (candidate)
                              (string-prefix-p prefix candidate))
                            candidates))))))

(defun indium-repl--complete-or-indent ()
  "Complete or indent at point."
  (interactive)
  (if (company-manual-begin)
      (company-complete-common)
    (indent-according-to-mode)))

(defun indium-repl-company-prefix ()
  "Prefix for company."
  (and (or (eq major-mode 'indium-repl-mode)
           (bound-and-true-p indium-interaction-mode))
       (or (company-grab-symbol-cons "\\." 1)
           'stop)))

(defvar indium-repl-mode-hook nil
  "Hook executed when entering `indium-repl-mode'.")

(defvar indium-repl-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map [return] #'indium-repl-return)
    (define-key map "\C-m" #'indium-repl-return)
    (define-key map (kbd "TAB") #'indium-repl--complete-or-indent)
    (define-key map [mouse-1] #'indium-follow-link)
    (define-key map (kbd "C-<return>") #'newline)
    (define-key map (kbd "C-c M-i") #'indium-repl-inspect)
    (define-key map (kbd "C-c C-o") #'indium-repl-clear-output)
    (define-key map (kbd "C-c C-z") #'indium-repl-pop-buffer)
    (define-key map (kbd "C-c C-q") #'indium-maybe-quit)
    (define-key map (kbd "M-p") #'indium-repl-previous-input)
    (define-key map (kbd "M-n") #'indium-repl-next-input)
    (define-key map (kbd "C-<up>") #'indium-repl-previous-input)
    (define-key map (kbd "C-<down>") #'indium-repl-next-input)
    (easy-menu-define indium-repl-mode-menu map
      "Menu for Indium REPL"
      '("Indium REPL"
        ["Clear output" indium-repl-clear-output]
        ["Inspect" indium-repl-inspect]
        "--"
        ["Switch to source buffer" indium-repl-pop-buffer]
        "--"
        ["Quit" indium-maybe-quit]))
    map))

(define-derived-mode indium-repl-mode fundamental-mode "JS-REPL"
  "Major mode for indium REPL interactions.

\\{indium-repl-mode-map}"
  (font-lock-add-keywords nil '(indium-repl--fontify-output))
  (setq-local company-backends '(company-indium-repl))
  (company-mode 1))

(defun indium-repl--fontify-output (&rest _)
  "Fontify JS code output."
  (let* ((start indium-repl-input-start-marker)
	 (end (point-max))
	 (repl-buffer (current-buffer))
	 (string (buffer-substring-no-properties start end)))
    (with-current-buffer
	(get-buffer-create " indium-fontification ")
      (let ((inhibit-modification-hooks nil))
	(js-mode)
	(erase-buffer)
	(insert string " ")
	(font-lock-ensure)
	(let ((pos (point-min)) next)
	  (while (setq next (next-property-change pos))
	    ;; Handle additional properties from font-lock, so as to
	    ;; preserve, e.g., composition.
	    (dolist (prop (cons 'face font-lock-extra-managed-props))
	      (let ((new-prop (get-text-property pos prop)))
		(put-text-property
		 (+ start (1- pos)) (1- (+ start next)) prop new-prop
		 repl-buffer)))
	    (setq pos next)))))))

(add-hook 'indium-client-connected-hook #'indium-repl-setup)
(add-hook 'indium-client-log-hook #'indium-repl-emit-console-message)

(provide 'indium-repl)
;;; indium-repl.el ends here
