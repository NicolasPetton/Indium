;;; indium-debugger.el --- Indium debugger               -*- lexical-binding: t; -*-

;; Copyright (C) 2016-2018  Nicolas Petton

;; Author: Nicolas Petton <nicolas@petton.fr>
;; Keywords: tools

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

;; - always evaluate on the current frame if any (check for inspection, etc.)

;;; Code:

(require 'seq)
(require 'map)

(require 'thingatpt)

(require 'easymenu)

(require 'indium-structs)
(require 'indium-inspector)
(require 'indium-repl)
(require 'indium-render)
(require 'indium-debugger-locals)
(require 'indium-debugger-litable)

(defgroup indium-debugger nil
  "JavaScript debugger"
  :prefix "indium-debugger-"
  :group 'indium)

(defcustom indium-debugger-major-mode
  #'js-mode
  "Major mode used in debugger buffers."
  :group 'indium-debugger
  :type 'function)

(defcustom indium-debugger-blackbox-regexps nil
  "List of file path regexps to blackbox when debugging.
Blackboxed scripts will be ignored (stepped out) when stepping in
from the debugger."
  :type '(repeat string))

(defcustom indium-debugger-inspect-when-eval nil
  "When non-nil, use inspect as a default eval when debugging."
  :type 'boolean)

(defvar indium-debugger-current-frame nil
  "Currently selected frame in the debugger.")

(defvar indium-debugger-frames nil
  "Call frames of the current debugger session.")

(defvar indium-debugger-buffer nil "Buffer used for debugging JavaScript sources.")

(defvar indium-debugger-message nil "Message to be displayed in the echo area.")
(make-local-variable 'indium-debugger-message)

(defconst indium-debugger-fringe-arrow-string
  #("." 0 1 (display (left-fringe right-triangle)))
  "Used as an overlay's before-string prop to place a fringe arrow.")

(defvar indium-debugger-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map " " #'indium-debugger-step-over)
    (define-key map (kbd "i") #'indium-debugger-step-into)
    (define-key map (kbd "o") #'indium-debugger-step-out)
    (define-key map (kbd "c") #'indium-debugger-resume)
    (define-key map (kbd "l") #'indium-debugger-locals)
    (define-key map (kbd "s") #'indium-debugger-stack-frames)
    (define-key map (kbd "q") #'indium-debugger-resume)
    (define-key map (kbd "h") #'indium-debugger-here)
    (define-key map (kbd "e") #'indium-debugger-evaluate)
    (define-key map (kbd "n") #'indium-debugger-next-frame)
    (define-key map (kbd "p") #'indium-debugger-previous-frame)
    (easy-menu-define indium-debugger-mode-menu map
      "Menu for Indium debugger"
      '("Indium Debugger"
        ["Resume" indium-debugger-resume]
        ["Step over" indium-debugger-step-over]
        ["Step into" indium-debugger-step-into]
        ["Step out" indium-debugger-step-out]
        ["Jump here" indium-debugger-here]
        "--"
        ["Inspect locals" indium-debugger-locals]
        ["Show stack" indium-debugger-stack-frames]
        "--"
        ["Evaluate" indium-debugger-evaluate]
        "--"
        ["Jump to the next frame" indium-debugger-next-frame]
        ["Jump to the previous frame" indium-debugger-previous-frame]))
    map))

(define-minor-mode indium-debugger-mode
  "Minor mode for debugging JS scripts.

\\{indium-debugger-mode-map}"
  :group 'indium
  :lighter " JS-debug"
  :keymap indium-debugger-mode-map
  (if indium-debugger-mode
      (add-hook 'pre-command-hook #'indium-debugger-refresh-echo-area nil t)
    (remove-hook 'pre-command-hook #'indium-debugger-refresh-echo-area t)))

(defun indium-debugger-paused (frames reason &optional description)
  "Handle execution pause.
Setup the debugging stack FRAMES when the execution has paused.
Display REASON in the echo area with an help message.

If DESCRIPTION is non-nil, display it in an overlay describing
the exception."
  (indium-debugger-set-frames frames)
  (indium-debugger-select-frame (car frames))
  (when description
    (indium-debugger-litable-add-exception-overlay description))
  (indium-debugger-show-help-message reason))

(defun indium-debugger-resumed (&rest _args)
  "Handle resumed execution.
Unset the debugging context and turn off indium-debugger-mode."
  (message "Execution resumed")
  (indium-debugger-unset-frames)
  (seq-doseq (buf (seq-filter (lambda (buf)
                                (with-current-buffer buf
                                  indium-debugger-mode))
                              (buffer-list)))
    (with-current-buffer buf
      (when overlay-arrow-position
	(set-marker overlay-arrow-position nil (current-buffer)))
      (indium-debugger-unset-current-buffer)
      (indium-debugger-litable-unset-buffer)))
    (let ((locals-buffer (indium-debugger-locals-get-buffer))
	  (frames-buffer (indium-debugger-frames-get-buffer)))
    (when locals-buffer (kill-buffer locals-buffer))
    (when frames-buffer (kill-buffer frames-buffer))))

(defun indium-debugger-next-frame ()
  "Jump to the next frame in the frame stack."
  (interactive)
  (indium-debugger--jump-to-frame 'forward))

(defun indium-debugger-previous-frame ()
  "Jump to the previous frame in the frame stack."
  (interactive)
  (indium-debugger--jump-to-frame 'backward))

(defun indium-debugger-stack-frames ()
  "List the stack frames in a separate buffer and switch to it."
  (interactive)
  (let ((buf (indium-debugger-frames-get-buffer-create))
        (inhibit-read-only t))
    (with-current-buffer buf
      (indium-debugger-frames-list indium-debugger-frames
				   indium-debugger-current-frame))
    (pop-to-buffer buf)))

(defun indium-debugger--jump-to-frame (direction)
  "Jump to the next frame in DIRECTION.
DIRECTION is `forward' or `backward' (in the frame list)."
  (let* ((current-position (seq-position indium-debugger-frames
					 indium-debugger-current-frame))
         (step (pcase direction
                 (`forward -1)
                 (`backward 1)))
         (position (+ current-position step)))
    (when (>= position (seq-length indium-debugger-frames))
      (user-error "End of frames"))
    (when (< position 0)
      (user-error "Beginning of frames"))
    (indium-debugger-select-frame (seq-elt indium-debugger-frames position))))

(defun indium-debugger-select-frame (frame)
  "Make FRAME the current debugged stack frame.

Setup a debugging buffer for the current stack FRAME and switch
to that buffer.

If no local file exists for the FRAME, ask the user if the remote
source for that frame should be downloaded.  If not, resume the
execution."
  (setq indium-debugger-current-frame frame)
  (switch-to-buffer (indium-debugger-get-buffer-create))
  (if buffer-file-name
      (indium-debugger-setup-buffer-with-file)
    (progn
      (if (yes-or-no-p "No file found for debugging (sourcemap issue?), download script source (might be slow)?")
	  (progn
	    (message "Downloading script source for debugging...")
	    (indium-client-get-frame-source
	     frame
	     (lambda (source)
	       (with-current-buffer (indium-debugger-get-buffer-create)
		 (indium-debugger-setup-buffer-with-source source))
	       (message "Downloading script source for debugging...done!"))))
	(indium-client-resume)))))

(defun indium-debugger-setup-buffer-with-file ()
  "Setup the current buffer for debugging."
  (when (buffer-modified-p)
    (revert-buffer nil nil t))
  (indium-debugger--goto-current-frame)
  (indium-debugger-litable-setup-buffer))

(defun indium-debugger-setup-buffer-with-source (source)
  "Setup the current buffer with the frame SOURCE."
  (unless (string= (buffer-substring-no-properties (point-min) (point-max))
                   source)
    (let ((inhibit-read-only t))
      (erase-buffer)
      (insert source)))
  (indium-debugger--goto-current-frame)
  (indium-debugger-litable-setup-buffer))

(defun indium-debugger--goto-current-frame ()
  "Move the point to the current stack frame position in the current buffer."
  (let* ((location (indium-frame-location indium-debugger-current-frame)))
    (goto-char (point-min))
    (forward-line (1- (indium-location-line location)))
    (forward-char (indium-location-column location)))
  (indium-debugger-setup-overlay-arrow)
  (indium-debugger-highlight-node)
  (indium-debugger-locals-maybe-refresh)
  (indium-debugger-frames-maybe-refresh))

(defun indium-debugger-show-help-message (&optional reason)
  "Display a help message with REASON in the echo-area."
  (setq indium-debugger-message
        (concat (propertize (or reason "")
                            'face 'font-lock-warning-face)
                " "
                (propertize "SPC"
                            'face 'font-lock-keyword-face)
                " over "
                (propertize "i"
                            'face 'font-lock-keyword-face)
                "nto "
                (propertize "o"
                            'face 'font-lock-keyword-face)
                "ut "
                (propertize "c"
                            'face 'font-lock-keyword-face)
                "ontinue "
                (propertize "h"
                            'face 'font-lock-keyword-face)
                "ere "
                (propertize "l"
                            'face 'font-lock-keyword-face)
                "ocals "
                (propertize "e"
                            'face 'font-lock-keyword-face)
                "val "
                (propertize "s"
                            'face 'font-lock-keyword-face)
                "tack "
                (propertize "n"
                            'face 'font-lock-keyword-face)
                "ext "
                (propertize "p"
                            'face 'font-lock-keyword-face)
                "rev"))
  (indium-debugger-refresh-echo-area))

(defun indium-debugger-refresh-echo-area ()
  "Refresh the echo area as motion commands clear the echo area."
  (message indium-debugger-message))

(defun indium-debugger-setup-overlay-arrow ()
  "Setup the overlay pointing to the current debugging line."
  (let ((pos (line-beginning-position)))
    (setq overlay-arrow-string "=>")
    (setq overlay-arrow-position (make-marker))
    (set-marker overlay-arrow-position pos (current-buffer))))

(defun indium-debugger-highlight-node ()
  "Highlight the current AST node where the execution has paused."
  (let ((beg (point))
        (end (line-end-position)))
    (indium-debugger-remove-highlights)
    (overlay-put (make-overlay beg end)
                 'face 'indium-highlight-face)))

(defun indium-debugger-remove-highlights ()
  "Remove all debugging highlighting overlays from the current buffer."
  (remove-overlays (point-min) (point-max) 'face 'indium-highlight-face))

(defun indium-debugger-top-frame ()
  "Return the top frame of the current debugging context."
  (car indium-debugger-frames))

(defun indium-debugger-step-into ()
  "Request a step into."
  (interactive)
  (indium-client-step-into))

(defun indium-debugger-step-over ()
  "Request a step over."
  (interactive)
  (indium-client-step-over))

(defun indium-debugger-step-out ()
  "Request a step out."
  (interactive)
  (indium-client-step-out))

(defun indium-debugger-resume ()
  "Request the runtime to resume the execution."
  (interactive)
  (indium-client-resume))

(defun indium-debugger-here ()
  "Request the runtime to resume the execution until the point.
When the position of the point is reached, pause the execution."
  (interactive)
  (indium-client-continue-to-location (indium-location-at-point)))

(defun indium-debugger-switch-to-debugger-buffer ()
  "Switch to the debugger buffer.
If there is no debugging session, signal an error."
  (unless indium-debugger-current-frame
    (user-error "No debugger to switch to"))
  (indium-debugger-select-frame indium-debugger-current-frame))

(defun indium-debugger-evaluate (expression)
  "Prompt for EXPRESSION to be evaluated.
Evaluation happens in the context of the current call frame.

When called with a prefix argument, or when
`indium-debugger-inspect-when-eval' is non-nil, inspect the
result of the evaluation if possible."
  (interactive (list
                (let ((default (if (region-active-p)
                                   (buffer-substring-no-properties (mark) (point))
                                 (thing-at-point 'symbol))))
                  (read-string (format "Evaluate on frame: (%s): " default)
                               nil nil default))))
  (indium-client-evaluate expression
			   (lambda (value)
			     (let ((inspect (and (or indium-debugger-inspect-when-eval
						     current-prefix-arg)
						 (map-elt value 'objectid))))
			       (if inspect
				   (indium-inspector-inspect value)
				 (message "%s" (indium-render-remote-object-to-string value)))))))

;; Debugging context

(defun indium-debugger-set-frames (frames)
  "Set the debugger FRAMES."
  (setq indium-debugger-frames frames)
  (setq indium-debugger-current-frame (car frames)))

(defun indium-debugger-unset-frames ()
  "Remove debugging information from the current connection."
  (setq indium-debugger-frames nil)
  (setq indium-debugger-current-frame nil))

(defun indium-debugger-get-current-scopes ()
  "Return the scope of the current stack frame."
  (and indium-debugger-current-frame
       (indium-frame-scope-chain indium-debugger-current-frame)))

(defun indium-debugger-get-scopes-properties (scopes callback)
  "Request a list of all properties in SCOPES.
CALLBACK is evaluated with the result."
  (seq-do (lambda (scope)
            (indium-debugger-get-scope-properties scope callback))
          scopes))

(defun indium-debugger-get-scope-properties (scope callback)
  "Request the properties of SCOPE and evaluate CALLBACK.
CALLBACK is evaluated with two arguments, the properties and SCOPE."
  (let-alist scope
    (indium-client-get-properties (indium-scope-id scope)
				  (lambda (properties)
				    (funcall callback properties scope)))))

(defun indium-debugger-get-buffer-create ()
  "Create a debugger buffer for the current connection and return it.

If a buffer already exists, just return it."
  (let* ((location (indium-frame-location indium-debugger-current-frame))
	 (file (indium-location-file location))
	 (buf (if (and file (file-regular-p file))
                  (find-file file)
		(get-buffer-create (indium-debugger--buffer-name-no-file)))))
    (indium-debugger-setup-buffer buf)
    buf))

(defun indium-debugger--buffer-name-no-file ()
  "Return the name of a debugger buffer.
This name should used when no local file can be found for a stack
frame."
  "*JS Debugger*")

(defun indium-debugger-setup-buffer (buffer)
  "Setup BUFFER for debugging."
  (with-current-buffer buffer
    (unless (or buffer-file-name
                (eq major-mode indium-debugger-major-mode))
      (funcall indium-debugger-major-mode))
    (indium-debugger-mode 1)
    (when (eq major-mode 'js2-mode)
      (js2-reparse))
    (read-only-mode)))

(defun indium-debugger-unset-current-buffer ()
  "Unset `indium-debugger-mode from the current buffer'."
  (indium-debugger-remove-highlights)
  (when overlay-arrow-position
    (set-marker overlay-arrow-position nil (current-buffer)))
  (indium-debugger-mode -1)
  (read-only-mode -1)
  (indium-debugger-litable-unset-buffer))

;; Frame listing

(defun indium-debugger-frames-maybe-refresh ()
  "When a buffer listing the stack frames is open, refresh it."
  (interactive)
  (let ((buf (indium-debugger-frames-get-buffer))
        (inhibit-read-only t))
    (when buf
      (with-current-buffer buf
        (indium-debugger-frames-list indium-debugger-frames
				     indium-debugger-current-frame)))))

(defun indium-debugger-frames-list (frames &optional current-frame)
  "Render the list of stack frames FRAMES.
CURRENT-FRAME is the current stack frame in the debugger."
  (save-excursion
    (erase-buffer)
    (indium-render-header "Debugger stack")
    (newline 2)
    (seq-doseq (frame frames)
      (indium-render-frame
       frame
       (eq current-frame frame))
      (newline))))

(defun indium-debugger-frames-select-frame (frame)
  "Select FRAME and switch to the corresponding debugger buffer."
  (interactive)
  (indium-debugger-select-frame frame))

(defun indium-debugger-frames-next-frame ()
  "Go to the next frame in the stack."
  (interactive)
  (indium-debugger-frames-goto-next 'next))

(defun indium-debugger-frames-previous-frame ()
  "Go to the previos frame in the stack."
  (interactive)
    (indium-debugger-frames-goto-next 'previous))

(defun indium-debugger-frames-goto-next (direction)
  "Go to the next frame in DIRECTION."
  (let ((next (eq direction 'next)))
    (forward-line (if next 1 -1))
    (back-to-indentation)
    (while (and (not (if next
                         (eobp)
                       (bobp)))
                (not (get-text-property (point) 'indium-action)))
      (forward-char (if next 1 -1)))))

(defun indium-debugger-frames-get-buffer ()
  "Return the buffer listing frames for the current connection.
If no buffer is found, return nil."
  (get-buffer (indium-debugger-frames-buffer-name)))

(defun indium-debugger-frames-buffer-name ()
  "Return the name of the frames buffer for the current connection."
  "*JS Frames*")

(defun indium-debugger-frames-get-buffer-create ()
  "Create a buffer for listing frames unless one exists, and return it."
  (let ((buf (indium-debugger-frames-get-buffer)))
    (unless buf
      (setq buf (generate-new-buffer (indium-debugger-frames-buffer-name)))
      (indium-debugger-frames-setup-buffer buf))
    buf))

(defun indium-debugger-frames-setup-buffer (buffer)
  "Setup the frames BUFFER."
  (with-current-buffer buffer
    (indium-debugger-frames-mode)
    (setq-local truncate-lines nil)))

(defvar indium-debugger-frames-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map [return] #'indium-follow-link)
    (define-key map (kbd "C-m") #'indium-follow-link)
    (define-key map (kbd "n") #'indium-debugger-frames-next-frame)
    (define-key map (kbd "p") #'indium-debugger-frames-previous-frame)
    (define-key map [tab] #'indium-debugger-frames-next-frame)
    (define-key map [backtab] #'indium-debugger-frames-previous-frame)
    map))

(define-derived-mode indium-debugger-frames-mode special-mode "Frames"
  "Major mode visualizind and navigating the JS stack.

\\{indium-debugger-frames--mode-map}"
  (setq buffer-read-only t)
  (font-lock-ensure)
  (read-only-mode))

(add-hook 'indium-client-debugger-paused-hook #'indium-debugger-paused)
(add-hook 'indium-client-debugger-resumed-hook #'indium-debugger-resumed)

(provide 'indium-debugger)
;;; indium-debugger.el ends here
