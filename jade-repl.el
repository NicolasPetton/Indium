;;; jade-repl.el --- JavaScript REPL connected to a browser tab  -*- lexical-binding: t; -*-

;; Copyright (C) 2016  Nicolas Petton

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

(require 'company)
(require 'jade-render)
(require 'jade-faces)
(require 'map)
(require 'js)

(defgroup jade-repl nil
  "Interaction with the REPL."
  :prefix "jade-repl-"
  :group 'jade)

(defvar jade-repl-evaluate-hook nil
  "Hook run when input is evaluated in the repl.")

(defvar jade-repl-history nil "History of the REPL inputs.")
(make-variable-buffer-local 'jade-repl-history)
(defvar jade-repl-history-position -1 "Position in the REPL history.")
(make-variable-buffer-local 'jade-repl-history-position)

(defvar-local jade-repl-input-start-marker nil)
(defvar-local jade-repl-prompt-start-marker nil)
(defvar-local jade-repl-output-start-marker nil)
(defvar-local jade-repl-output-end-marker nil)

(defmacro jade-save-marker (marker &rest body)
  "Save MARKER and execute BODY."
  (declare (indent 1) (debug t))
  (let ((pos (make-symbol "pos")))
    `(let ((,pos (marker-position ,marker)))
       (prog1 (progn . ,body)
         (set-marker ,marker ,pos)))))

(defun jade-repl-get-buffer-create (connection)
  "Return a REPL buffer for CONNECTION.
If no buffer exists, create one."
  (let* ((url (map-elt connection 'url))
         (buf (get-buffer-create (jade-repl-buffer-name url))))
    (jade-repl-setup-buffer buf connection)
    buf))

(defun jade-repl-get-buffer ()
  "Return the REPL buffer, or nil."
  (get-buffer (jade-repl-buffer-name)))

(defun jade-repl-buffer-name (&optional url)
  "Return the name of the REPL buffer for URL.
If URL is nil, use the current connection."
  (concat "*JS REPL " (or url (map-elt jade-connection 'url)) "*"))

(defun jade-repl-setup-buffer (buffer connection)
  "Setup the REPL BUFFER for CONNECTION."
  (with-current-buffer buffer
    (jade-repl-mode)
    (setq-local jade-connection connection)
    (jade-repl-setup-markers)
    (jade-repl-mark-output-start)
    (jade-repl-insert-prompt)
    (jade-repl-mark-input-start)
    (jade-repl-emit-console-message `((text . ,(jade-repl--welcome-message))))))

(defun jade-repl--welcome-message ()
  "Return the welcome message displayed in new REPL buffers."
  (format
   (substitute-command-keys
    "Welcome to Jade!
Connected to %s @ %s

Getting started:

- Press <\\[jade-repl-return]> on links to open an inspector
- Press <\\[jade-repl-previous-input]> and <\\[jade-repl-next-input]> to navigate in the history
- Use <\\[jade-scratch]> to open a scratch buffer for JS evaluation
- Press <\\[describe-mode]> to see a list of available keybindings
- Press <\\[jade-repl-clear-output]> to clear the output

To disconnect from the JavaScript process, press <\\[jade-quit]>.
Doing this will also close all inspectors and debugger buffers
connected to the process.

")
   (map-elt jade-connection 'backend)
   (map-elt jade-connection 'url)))


(defun jade-repl-setup-markers ()
  "Setup the initial markers for the current REPL buffer."
  (dolist (marker '(jade-repl-prompt-start-marker
                    jade-repl-output-start-marker
                    jade-repl-output-end-marker
                    jade-repl-input-start-marker))
    (set marker (make-marker))
    (set-marker (symbol-value marker) (point))))

(defun jade-repl-mark-output-start ()
  "Mark the output start."
  (set-marker jade-repl-output-start-marker (point))
  (set-marker jade-repl-output-end-marker (point)))

(defun jade-repl-mark-input-start ()
  "Mark the input start."
  (set-marker jade-repl-input-start-marker (point)))

(defun jade-repl-insert-prompt ()
  "Insert the prompt in the REPL buffer."
  (goto-char jade-repl-input-start-marker)
  (jade-save-marker jade-repl-output-start-marker
    (jade-save-marker jade-repl-output-end-marker
      (unless (bolp)
        (insert-before-markers "\n"))
      (insert-before-markers "js> ")
      (let ((beg (save-excursion
                   (beginning-of-line)
                   (point)))
            (end (point)))
        (set-text-properties beg end
                             '(font-lock-face jade-repl-prompt-face
                                              read-only t
                                              intangible t
                                              field jade-repl-prompt
                                              rear-nonsticky (read-only font-lock-face intangible field)))
        (set-marker jade-repl-prompt-start-marker beg)))))

(defun jade-repl-return ()
  "Depending on the position of point, jump to a reference of evaluate the input."
  (interactive)
  (cond
   ((get-text-property (point) 'jade-reference) (jade-follow-link))
   ((get-text-property (point) 'jade-action) (jade-perform-action))
   ((jade-repl--in-input-area-p) (jade-repl-evaluate (jade-repl--input-content)))
   (t (error "No input or action at point"))))

(defun jade-repl-inspect ()
  "Inspect the result of the evaluation of the input at point."
  (interactive)
  (jade-backend-evaluate (jade-backend)
                         (jade-repl--input-content)
                         (lambda (result _error)
                           (jade-inspector-inspect result))))

(defun jade-repl--input-content ()
  "Return the content of the current input."
  (buffer-substring-no-properties jade-repl-input-start-marker (point-max)))

(defun jade-repl--in-input-area-p ()
  "Return t if in input area."
  (<= jade-repl-input-start-marker (point)))

(declare-function #'jade-backend-evaluate "jade")

(defun jade-repl-evaluate (string)
  "Evaluate STRING in the browser tab and emit the output."
  (push string jade-repl-history)
  (jade-backend-evaluate (jade-backend) string #'jade-repl-emit-value)
  ;; move the output markers so that output is put after the current prompt
  (save-excursion
    (goto-char (point-max))
    (set-marker jade-repl-output-start-marker (point))
    (set-marker jade-repl-output-end-marker (point))))

(defun jade-repl-emit-value (value error)
  "Emit a string representation of VALUE.
When ERROR is non-nil, display VALUE as an error."
  (with-current-buffer (jade-repl-get-buffer)
    (save-excursion
      (goto-char (point-max))
      (insert-before-markers "\n")
      (set-marker jade-repl-output-start-marker (point))
      (when error (jade-repl--emit-logging-level "error"))
      (jade-render-value value)
      (insert "\n")
      (jade-repl-mark-input-start)
      (set-marker jade-repl-output-end-marker (point)))
    (jade-repl-insert-prompt)
    (run-hooks 'jade-repl-evaluate-hook)))

(defun jade-repl-emit-console-message (message)
  "Emit a console message.
MESSAGE is a map (alist/hash-table) with the following keys:
  level		severity level (can be log, warning, error, debug)
  text		message text to be displayed
  type		type of message
  url		url of the message origin
  line		line number in the resource that generated this message
  parameters	message parameters in case of the formatted message

MESSAGE must contain `text' or `parameters.'.  Other fields are
optional."
  (with-current-buffer (jade-repl-get-buffer)
    (save-excursion
      (let ((text (map-elt message 'text))
            (level (or (map-elt message 'level) "")))
        (goto-char jade-repl-output-end-marker)
        (set-marker jade-repl-output-start-marker (point))
        (insert "\n")
        (jade-repl--emit-logging-level level)
        (jade-repl--emit-message-values message)
        (set-marker jade-repl-output-end-marker (point))
        (unless (eolp)
          (insert "\n"))
        ;; TODO: add an option to disable it
        ;; when we get an error, also display it in the echo area for
        ;; convenience
        (when (jade-repl--message-level-error-p level)
          (message text))))))

(defun jade-repl--emit-message-values (message)
  "Emit all values of console MESSAGE."
  (let ((text (map-elt message 'text))
        (values (map-elt message 'parameters))
        (url (map-elt message 'url))
        (line (map-elt message 'line)))
    (when (seq-empty-p values)
      (setq values `(((type . "string")
                      (description . ,text)))))
    (jade-render-values values "\n")
    (jade-repl--emit-message-url-line url line)))

(defun jade-repl--message-level-error-p (level)
  (string= level "error"))

(defun jade-repl-level-face (level)
  "Return the face to be used to render a console message from its LEVEL."
  (if (jade-repl--message-level-error-p level)
      'jade-repl-error-face
    'jade-repl-stdout-face))

(defun jade-repl--emit-logging-level (level)
  (unless (string-empty-p level)
    (insert
     (ansi-color-apply
      (propertize (format "%s:" level)
                  'font-lock-face (jade-repl-level-face level)
                  'rear-nonsticky '(font-lock-face)))
     " ")))

(defun jade-repl--emit-message-url-line (url line)
  (unless (seq-empty-p url)
    (insert "\nMessage from "
            (propertize (format "%s:%s" (file-name-nondirectory url) line)
                        'font-lock-face 'jade-link-face
                        'jade-action (lambda ()
                                       (browse-url url))
                        'rear-nonsticky '(font-lock-face jade-action)))))

(defun jade-repl-next-input ()
  "Insert the content of the next input in the history."
  (interactive)
  (jade-repl--history-replace 'forward))

(defun jade-repl-previous-input ()
  "Insert the content of the previous input in the history."
  (interactive)
  (jade-repl--history-replace 'backward))

(defun jade-repl--history-replace (direction)
  "Replace the current input with one the next one in DIRECTION.
DIRECTION is `forward' or `backard' (in the history list)."
  (let* ((history (seq-reverse jade-repl-history))
         (search-in-progress (or (eq last-command 'jade-repl-previous-input)
                                 (eq last-command 'jade-repl-next-input)))
         (step (pcase direction
                 (`forward 1)
                 (`backward -1)))
         (pos (or (and search-in-progress (+ jade-repl-history-position step))
                  (1- (seq-length history)))))
    (unless (>= pos 0)
      (user-error "Beginning of history"))
    (unless (< pos (seq-length history))
      (user-error "End of history"))
    (setq jade-repl-history-position pos)
    (jade-repl--replace-input (seq-elt history pos))))

(defun jade-repl--replace-input (input)
  "Replace the current input with INPUT."
  (goto-char (point-max))
  (delete-region jade-repl-input-start-marker (point))
  (insert input))

(defun jade-repl-clear-output ()
  "Clear all output contents of the current buffer."
  (interactive)
  (let ((inhibit-read-only t))
    (save-excursion
      (goto-char (point-min))
      (delete-region (point) jade-repl-prompt-start-marker))))

(defun jade-repl--handle-connection-closed ()
  "Display a message when the connection is closed."
  (with-current-buffer (jade-repl-get-buffer)
    (save-excursion
      (goto-char (point-max))
      (insert-before-markers "\n")
      (set-marker jade-repl-output-start-marker (point))
      (insert "Connection closed. ")
      (jade-repl--insert-connection-buttons)
      (insert "\n")
      (set-marker jade-repl-input-start-marker (point))
      (set-marker jade-repl-output-end-marker (point)))
    (jade-repl-insert-prompt)))

(defun jade-repl--insert-connection-buttons ()
  (jade-render-button "Reconnect" #'jade-reconnect)
  (insert " or ")
  (jade-render-button "close all buffers" #'jade-quit)
  (insert "."))

(defun company-jade-repl (command &optional arg &rest _args)
  "Jade REPL backend for company-mode.
See `company-backends' for more info about COMMAND and ARG."
  (interactive (list 'interactive))
  (cl-case command
    (interactive (company-begin-backend 'company-jade-repl))
    (prefix (jade-repl-company-prefix))
    (ignore-case t)
    (sorted t)
    (candidates (cons :async
                      (lambda (callback)
                        (jade-repl-get-completions arg callback))))))

(defun jade-repl-get-completions (arg callback)
  "Get the completion list matching the prefix ARG.
Evaluate CALLBACK with the completion candidates."
  (let ((expression (buffer-substring-no-properties jade-repl-input-start-marker
                                                    (point-max-marker))))
    (jade-backend-get-completions (jade-backend) expression arg callback)))

(defun jade-repl--complete-or-indent ()
  "Complete or indent at point."
  (interactive)
  (if (company-manual-begin)
      (company-complete-common)
    (indent-according-to-mode)))

(defun jade-repl-company-prefix ()
  "Prefix for company."
  (and (eq major-mode 'jade-repl-mode)
       (or (company-grab-symbol-cons "\\." 1)
           'stop)))

(defvar jade-repl-mode-hook nil
  "Hook executed when entering `jade-repl-mode'.")

(declare 'jade-quit)

(defvar jade-repl-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map [return] #'jade-repl-return)
    (define-key map "\C-m" #'jade-repl-return)
    (define-key map (kbd "TAB") #'jade-repl--complete-or-indent)
    (define-key map [mouse-1] #'jade-follow-link)
    (define-key map (kbd "C-<return>") #'newline)
    (define-key map (kbd "C-c M-i") #'jade-repl-inspect)
    (define-key map (kbd "C-c C-o") #'jade-repl-clear-output)
    (define-key map (kbd "C-c C-q") #'jade-quit)
    (define-key map (kbd "M-p") #'jade-repl-previous-input)
    (define-key map (kbd "M-n") #'jade-repl-next-input)
    map))

(define-derived-mode jade-repl-mode fundamental-mode "JS-REPL"
  "Major mode for jade REPL interactions.

\\{jade-repl-mode-map}"
  (setq-local font-lock-defaults (list js--font-lock-keywords))
  (setq-local syntax-propertize-function #'js-syntax-propertize)
  (font-lock-ensure)
  (setq-local company-backends '(company-jade-repl))
  (company-mode 1)
  (setq-local comment-start "// ")
  (setq-local comment-end ""))

(provide 'jade-repl)
;;; jade-repl.el ends here
