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
(defvar jade-repl-history-position -1 "Position in the REPL history.")

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
    (jade-repl-mark-input-start)
    (jade-repl-insert-prompt)
    (jade-repl-emit-console-message
     (format "Welcome to Jade!\nConnected to %s @ %s\n"
             (map-elt jade-connection 'backend)
             (map-elt jade-connection 'url)))))

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
                         (lambda (result error)
                           (when error
                             (jade-repl-emit-value result error))
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
When ERROR is non-nil, use the error face."
  (with-current-buffer (jade-repl-get-buffer)
    (save-excursion
      (goto-char (point-max))
      (insert-before-markers "\n")
      (set-marker jade-repl-output-start-marker (point))
      (jade-render-value value error)
      (insert "\n")
      (set-marker jade-repl-input-start-marker (point))
      (set-marker jade-repl-output-end-marker (point)))
    (jade-repl-insert-prompt)
    (run-hooks 'jade-repl-evaluate-hook)))

(defun jade-repl-emit-console-message (string &optional level)
  "Emit a console message STRING.
LEVEL is a string representing the logging level, it can be
\"log\", \"warn\", \"debug\" or \"error\"."
  (with-current-buffer (jade-repl-get-buffer)
    (save-excursion
      (let* ((error (string= level "error"))
             (face (when error 'jade-repl-error-face))
             (message (if level
                          (concat level ": " string)
                        string)))
        (goto-char jade-repl-output-end-marker)
        (insert "\n")
        (set-marker jade-repl-output-start-marker (point))
        (insert
         (ansi-color-apply
          (propertize message
                      'font-lock-face (or face 'jade-repl-stdout-face)
                      'rear-nonsticky '(font-lock-face))))
        (set-marker jade-repl-output-end-marker (point))
        (unless (eolp)
          (insert "\n"))
        ;; when we get an error, also display it in the echo area for
        ;; convenience
        (when error (message string))))))

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
    (unless (> pos 0)
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
      (goto-char (point-min))
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
    (define-key map "\C-m"#'jade-repl-return)
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
