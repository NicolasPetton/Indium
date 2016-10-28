;;; jade-debugger.el --- Jade debugger               -*- lexical-binding: t; -*-

;; Copyright (C) 2016  Nicolas Petton

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

;;

;;; Code:

(require 'seq)
(require 'map)
(require 'jade-inspector)
(require 'jade-repl)
(require 'jade-interaction)
(require 'jade-render)

(defgroup jade-debugger nil
  "JavaScript debugger"
  :prefix "jade-debugger-"
  :group 'jade)

(defcustom jade-debugger-major-mode
  #'js-mode
  "Major mode used in debugger buffers."
  :group 'jade-debugger
  :type 'function)

(defvar jade-debugger-buffer nil "Buffer used for debugging JavaScript sources.")

(defvar jade-debugger-frames nil "List of frames of the current debugger context.")
(make-variable-buffer-local 'jade-debugger-frames)

(defvar jade-debugger-current-frame nil "Current frame of the debugger context.")
(make-variable-buffer-local 'jade-debugger-current-frame)

(defconst jade-debugger-fringe-arrow-string
  #("." 0 1 (display (left-fringe right-triangle)))
  "Used as an overlay's before-string prop to place a fringe arrow.")

(declare 'jade-backend-debugger-get-script-source)

(defun jade-debugger-paused (frames)
  (jade-debugger-get-buffer-create jade-connection frames)
  (jade-debugger-select-frame (car frames))
  (jade-debugger-show-help-message))

(defun jade-debugger-resumed (&rest _args)
  (let ((buf (jade-debugger-get-buffer)))
    (when buf
      (set-marker overlay-arrow-position nil (current-buffer))
      (remove-overlays))))

(defun jade-debugger-next-frame ()
  "Jump to the next frame in the frame stack."
  (interactive)
  (jade-debugger--jump-to-frame 'forward))

(defun jade-debugger-previous-frame ()
  "Jump to the previous frame in the frame stack."
  (interactive)
  (jade-debugger--jump-to-frame 'backward))

(defun jade-debugger--jump-to-frame (direction)
  "Jump to the next frame in DIRECTION.
DIRECTION is `forward' or `backward' (in the frame list)."
  (let* ((current-position (seq-position jade-debugger-frames jade-debugger-current-frame))
         (step (pcase direction
                 (`forward -1)
                 (`backward 1)))
         (position (+ current-position step)))
    (when (> position (seq-length jade-debugger-frames))
      (user-error "End of frames"))
    (when (< position 0)
      (user-error "Beginning of frames"))
    (jade-debugger-select-frame (seq-elt jade-debugger-frames position))))

(defun jade-debugger-select-frame (frame)
  "Switch the debugger buffer to the frame FRAME."
  (jade-backend-get-script-source (jade-backend)
                                  frame
                                  (lambda (source)
                                    (jade-debugger-switch-to-frame
                                     frame
                                     (map-nested-elt source '(result scriptSource))))))

(defun jade-debugger-switch-to-frame (frame source)
  (switch-to-buffer (jade-debugger-get-buffer))
  (jade-debugger-debug-frame frame source)
  (jade-debugger-locals-maybe-refresh)
  (jade-debugger-frames-maybe-refresh))

(defun jade-debugger-debug-frame (frame source)
  (let* ((location (map-elt frame 'location))
         (line (map-elt location 'lineNumber))
         (column (map-elt location 'columnNumber))
         (inhibit-read-only t))
    (setq jade-debugger-current-frame frame)
    (unless (string= (buffer-substring-no-properties (point-min) (point-max))
                     source)
      (erase-buffer)
      (insert source))
    (goto-char (point-min))
    (forward-line line)
    (forward-char column)
    (jade-debugger-setup-overlay-arrow)
    (jade-debugger-highlight-node)))

(defun jade-debugger-show-help-message ()
  "Display a help message in the echo-area."
  (let ((message (concat "["
                         (propertize "SPC"
                                     'face 'font-lock-keyword-face)
                         "]over "
                         "["
                         (propertize "i"
                                     'face 'font-lock-keyword-face)
                         "]nto "
                         "["
                         (propertize "o"
                                     'face 'font-lock-keyword-face)
                         "]ut "
                         "["
                         (propertize "c"
                                     'face 'font-lock-keyword-face)
                         "]ontinue "
                         "["
                         (propertize "h"
                                     'face 'font-lock-keyword-face)
                         "]ere "
                         "["
                         (propertize "l"
                                     'face 'font-lock-keyword-face)
                         "]ocals "
                         "["
                         (propertize "e"
                                     'face 'font-lock-keyword-face)
                         "]val... "
                         "["
                         (propertize "s"
                                     'face 'font-lock-keyword-face)
                         "]tack "
                         "["
                         (propertize "n"
                                     'face 'font-lock-keyword-face)
                         "]ext "
                         "["
                         (propertize "p"
                                     'face 'font-lock-keyword-face)
                         "]revious")))
    (message "Debug: %s" message)))

(defun jade-debugger-setup-overlay-arrow ()
  (let ((pos (line-beginning-position)))
    (setq overlay-arrow-string "=>")
    (setq overlay-arrow-position (make-marker))
    (set-marker overlay-arrow-position pos (current-buffer))))

(defun jade-debugger-highlight-node ()
  (let ((beg (point))
        (end (line-end-position)))
    (remove-overlays)
    (overlay-put (make-overlay beg end)
                 'face 'jade-highlight-face)))

(defun jade-debugger-top-frame ()
  "Return the top frame of the current debugging context."
  (car jade-debugger-frames))

(defun jade-debugger-step-into ()
  (interactive)
  (jade-backend-step-into (jade-backend)))

(defun jade-debugger-step-over ()
  (interactive)
  (jade-backend-step-over (jade-backend)))

(defun jade-debugger-step-out ()
  (interactive)
  (jade-backend-step-out (jade-backend)))

(defun jade-debugger-resume ()
  (interactive)
  (jade-backend-resume (jade-backend) #'jade-debugger-resumed)
  (let ((locals-buffer (jade-debugger-locals-get-buffer))
        (frames-buffer (jade-debugger-frames-get-buffer)))
    (when locals-buffer
      (kill-buffer locals-buffer))
    (when frames-buffer
      (kill-buffer frames-buffer))
    (kill-buffer (jade-debugger-get-buffer))))

(defun jade-debugger-here ()
  (interactive)
  (jade-backend-continue-to-location (jade-backend)
                                     `((scriptId . ,(map-nested-elt (jade-debugger-top-frame)
                                                                    '(location scriptId)))
                                       (lineNumber . ,(1- (count-lines (point-min) (point)))))))

(defun jade-debugger-evaluate (expression)
  "Prompt for EXPRESSION to be evaluated.
Evaluation happens in the context of the current call frame."
  (interactive "sEvaluate on frame: ")
  (jade-debugger-eval expression
                      (lambda (value _error)
                        (message (jade-description-string value)))))

(defun jade-debugger-eval-last-node ()
  "Evaluate the node before point."
  (interactive)
  (jade-debugger-evaluate (js2-node-string (jade-interaction-node-before-point))))

(defun jade-debugger-eval (expression callback)
  "Evaluate EXPRESSION and call CALLBACK with the returned value.
Evaluation happens in the context of the current call frame."
  (jade-backend-evaluate-on-frame (jade-backend)
                                  expression
                                  (jade-debugger-top-frame)
                                  callback))

(defun jade-debugger-inspect-last-node ()
  "Evaluate and inspect the node before point."
  (interactive)
  (jade-debugger-eval (js2-node-string (jade-interaction-node-before-point))
                      (lambda (result error)
                        (when error
                          (message "JS error: %s" result))
                        (jade-inspector-inspect result))))

(defun jade-debugger-get-buffer-create (connection frames)
  "Create a debugger buffer for CONNECTION and return it.

Locally set `jade-debugger-frames' to FRAMES.
If a buffer already exists, just return it."
  (let ((buf (jade-debugger-get-buffer)))
    (unless buf
      (setq buf (get-buffer-create (jade-debugger-buffer-name)))
      (jade-debugger-setup-buffer buf connection))
    (with-current-buffer buf
      (setq-local jade-debugger-frames frames))
    buf))

(defun jade-debugger-buffer-name ()
    (concat "*JS Debugger " (map-elt jade-connection 'url) "*"))

(defun jade-debugger-get-buffer ()
  (get-buffer (jade-debugger-buffer-name)))

(defun jade-debugger-setup-buffer (buffer connection)
  (with-current-buffer buffer
    (funcall jade-debugger-major-mode)
    (setq-local jade-connection connection)
    (jade-debugger-mode)
    (read-only-mode)))

(defvar jade-debugger-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map " " #'jade-debugger-step-over)
    (define-key map (kbd "i") #'jade-debugger-step-into)
    (define-key map (kbd "o") #'jade-debugger-step-out)
    (define-key map (kbd "c") #'jade-debugger-resume)
    (define-key map (kbd "l") #'jade-debugger-locals)
    (define-key map (kbd "s") #'jade-debugger-stack-frames)
    (define-key map (kbd "q") #'jade-debugger-resume)
    (define-key map (kbd "h") #'jade-debugger-here)
    (define-key map (kbd "e") #'jade-debugger-evaluate)
    (define-key map (kbd "n") #'jade-debugger-next-frame)
    (define-key map (kbd "p") #'jade-debugger-previous-frame)
    (define-key map (kbd "C-x C-e") #'jade-debugger-eval-last-node)
    (define-key map (kbd "C-c M-i") #'jade-debugger-inspect-last-node)
    map))

(define-minor-mode jade-debugger-mode
  "Minor mode for debugging JS scripts.

\\{jade-debugger-mode-map}"
  :group 'jade
  :lighter " JS-debug"
  :keymap jade-debugger-mode-map)

;;; Locals

(declare 'jade-backend-get-properties)

(defun jade-debugger-locals (&optional no-pop)
  "Inspect the local variables in the current stack frame's scope.
Unless NO-POP is non-nil, pop the locals buffer."
  (interactive)
  (let* ((buf (jade-debugger-locals-get-buffer-create))
        (inhibit-read-only t))
    (with-current-buffer buf
      (erase-buffer)))
  (seq-do (lambda (scope)
            (jade-backend-get-properties
             (jade-backend)
             (map-nested-elt scope '(object objectid))
             (lambda (properties)
               (jade-debugger-locals-render-properties properties scope no-pop))))
          ;; do not inspect the window object
          (seq-remove (lambda (scope)
                        (string= (map-elt scope 'type) "global"))
                      (map-elt jade-debugger-current-frame 'scope-chain))))

(defun jade-debugger-locals-maybe-refresh ()
  "When a local inspector is open, refresh it."
  (interactive)
  (let ((buf (jade-debugger-locals-get-buffer)))
    (when buf
      (jade-debugger-locals t))))

(defun jade-debugger-locals-render-properties (properties scope &optional no-pop)
  (let* ((buf (jade-debugger-locals-get-buffer-create))
         (inhibit-read-only t)
         (name (map-elt scope 'name))
         (type (map-elt scope 'type))
         (description (if (or (null name)
                              (string= name "undefined"))
                          type
                        name)))
    (with-current-buffer buf
      (save-excursion
        (goto-char (point-max))
        (jade-render-keyword description)
        (insert "\n\n")
        (jade-render-properties properties)
        (insert "\n")))
    (unless no-pop
      (pop-to-buffer buf))))

(defun jade-debugger-locals-get-buffer ()
  (get-buffer (jade-debugger-locals-buffer-name)))

(defun jade-debugger-locals-buffer-name ()
  (concat "*JS Debugger Locals " (map-elt jade-connection 'url) "*"))

(defun jade-debugger-locals-get-buffer-create ()
  "Create a locals buffer unless one exists, and return it."
  (let ((buf (jade-debugger-locals-get-buffer)))
    (unless buf
      (setq buf (generate-new-buffer (jade-debugger-locals-buffer-name)))
      (jade-debugger-locals-setup-buffer buf jade-connection))
    buf))

(defun jade-debugger-locals-setup-buffer (buffer connection)
  (with-current-buffer buffer
    (jade-debugger-locals-mode)
    (read-only-mode)
    (setq-local jade-connection connection)))

(defvar jade-debugger-locals-mode-map
  (let ((map (copy-keymap jade-inspector-mode-map)))
    (define-key map "g" nil)
    (define-key map "l" nil)
    map))

(define-derived-mode jade-debugger-locals-mode jade-inspector-mode "Locals"
  "Major mode for inspecting local variables in a scope-chain.

\\{jade-debugger-locals-mode-map}")

;;; Frame list

(defun jade-debugger-stack-frames ()
  "List the stack frames in a separate buffer and switch to it."
  (interactive)
  (let ((buf (jade-debugger-frames-get-buffer-create))
        (frames jade-debugger-frames)
        (current-frame jade-debugger-current-frame)
        (inhibit-read-only t))
    (with-current-buffer buf
     (jade-debugger-list-frames frames current-frame))
    (switch-to-buffer-other-window buf)))

(defun jade-debugger-frames-maybe-refresh ()
  "When a buffer listing the stack frames is open, refresh it."
  (interactive)
  (let ((buf (jade-debugger-frames-get-buffer))
        (frames jade-debugger-frames)
        (current-frame jade-debugger-current-frame)
        (inhibit-read-only t))
    (when buf
      (with-current-buffer buf
        (jade-debugger-list-frames frames current-frame)))))

(defun jade-debugger-list-frames (frames &optional current-frame)
  "Render the list of stack frames FRAME.
CURRENT-FRAME is the current stack frame in the debugger."
  (save-excursion
    (erase-buffer)
    (jade-render-header "Debugger stack")
    (newline 2)
    (seq-doseq (frame frames)
      (jade-render-frame frame
                         (jade-backend-get-script-url (jade-backend) frame)
                         (eq current-frame frame))
      (newline))))

(defun jade-debugger-frames-select-frame (frame)
  "Select FRAME and switch to the corresponding debugger buffer."
  (interactive)
  (let ((buf (current-buffer)))
    (switch-to-buffer-other-window (jade-debugger-get-buffer))
    (jade-debugger-select-frame frame)
    (switch-to-buffer buf)))

(defun jade-debugger-frames-next-frame ()
  (interactive)
  (jade-debugger-frames-goto-next 'next))

(defun jade-debugger-frames-previous-frame ()
  (interactive)
    (jade-debugger-frames-goto-next 'previous))

(defun jade-debugger-frames-goto-next (direction)
  (let ((next (eq direction 'next)))
    (forward-line (if next 1 -1))
    (back-to-indentation)
    (while (and (not (if next
                         (eobp)
                       (bobp)))
                (not (get-text-property (point) 'jade-action)))
      (forward-char (if next 1 -1)))))

(defun jade-debugger-frames-get-buffer ()
  "Return the buffer listing frames for the current connection.
If no buffer is found, return nil."
  (get-buffer (jade-debugger-frames-buffer-name)))

(defun jade-debugger-frames-buffer-name ()
  "Return the name of the frames buffer for the current connection."
  (concat "*JS Frames " (map-elt jade-connection 'url) "*"))

(defun jade-debugger-frames-get-buffer-create ()
  "Create a buffer for listing frames unless one exists, and return it."
  (let ((buf (jade-debugger-frames-get-buffer)))
    (unless buf
      (setq buf (generate-new-buffer (jade-debugger-frames-buffer-name)))
      (jade-debugger-frames-setup-buffer buf jade-connection))
    buf))

(defun jade-debugger-frames-setup-buffer (buffer connection)
  (with-current-buffer buffer
    (jade-debugger-frames-mode)
    (read-only-mode)
    (setq-local truncate-lines nil)
    (setq-local jade-connection connection)))

(defvar jade-debugger-frames-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map [return] #'jade-follow-link)
    (define-key map (kbd "C-m") #'jade-follow-link)
    (define-key map (kbd "n") #'jade-debugger-frames-next-frame)
    (define-key map (kbd "p") #'jade-debugger-frames-previous-frame)
    (define-key map [tab] #'jade-debugger-frames-next-frame)
    (define-key map [backtab] #'jade-debugger-frames-previous-frame)
    map))

(define-derived-mode jade-debugger-frames-mode special-mode "Frames"
  "Major mode visualizind and navigating the JS stack.

\\{jade-debugger-frames--mode-map}"
  (setq buffer-read-only t)
  (font-lock-ensure))

(provide 'jade-debugger)
;;; jade-debugger.el ends here
