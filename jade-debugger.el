;;; jade-debugger.el --- Jade debugger               -*- lexical-binding: t; -*-

;; Copyright (C) 2016-2017  Nicolas Petton

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
(require 'jade-inspector)
(require 'jade-repl)
(require 'jade-interaction)
(require 'jade-render)
(require 'jade-workspace)
(require 'jade-debugger-frames)
(require 'jade-debugger-locals)
(require 'jade-debugger-litable)

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

(defconst jade-debugger-fringe-arrow-string
  #("." 0 1 (display (left-fringe right-triangle)))
  "Used as an overlay's before-string prop to place a fringe arrow.")

(declare 'jade-backend-debugger-get-script-source)

(defun jade-debugger-paused (frames)
  (jade-debugger-setup-context frames (car frames))
  (jade-debugger-select-frame (car frames))
  (jade-debugger-show-help-message))

(defun jade-debugger-resumed (&rest _args)
  (seq-doseq (buf (seq-filter (lambda (buf)
                                (with-current-buffer buf
                                  jade-debugger-mode))
                              (buffer-list)))
    (with-current-buffer buf
      (set-marker overlay-arrow-position nil (current-buffer))
      (jade-debugger-remove-highlights)
      (jade-debugger-litable-unset-buffer))))

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
  (let* ((current-position (seq-position (jade-debugger-frames) (jade-debugger-current-frame)))
         (step (pcase direction
                 (`forward -1)
                 (`backward 1)))
         (position (+ current-position step)))
    (when (> position (seq-length (jade-debugger-frames)))
      (user-error "End of frames"))
    (when (< position 0)
      (user-error "Beginning of frames"))
    (jade-debugger-select-frame (seq-elt (jade-debugger-frames) position))))

(defun jade-debugger-select-frame (frame)
  "Make FRAME the current debugged stach frame.
Switch to the buffer for FRAME.

Try to find the file locally first using Jade worskspaces.  If a
local file cannot be found, get the remote source and open a new
buffer visiting it."
  (jade-debugger-set-current-frame frame)
  (jade-debugger-litable-setup-buffer)
  (switch-to-buffer (jade-debugger-get-buffer-create))
  (if buffer-file-name
      (jade-debugger-setup-buffer-with-file)
    (jade-backend-get-script-source
       (jade-backend)
       frame
       (lambda (source)
         (jade-debugger-setup-buffer-no-file
          (map-nested-elt source '(result scriptSource)))))))

(defun jade-debugger-setup-buffer-with-file ()
  "Setup the current buffer for debugging."
  (when (buffer-modified-p)
    (revert-buffer nil nil t))
  (jade-debugger-position-buffer))

(defun jade-debugger-setup-buffer-no-file (source)
  "Setup the current buffer with the frame source SOURCE."
  (unless (string= (buffer-substring-no-properties (point-min) (point-max))
                   source)
    (let ((inhibit-read-only t))
      (erase-buffer)
      (insert source)))
  (jade-debugger-position-buffer))

(defun jade-debugger-position-buffer ()
  (let* ((frame (jade-debugger-current-frame))
         (location (map-elt frame 'location))
         (line (map-elt location 'lineNumber))
         (column (map-elt location 'columnNumber)))
    (goto-char (point-min))
    (forward-line line)
    (forward-char column))
  (jade-debugger-setup-overlay-arrow)
  (jade-debugger-highlight-node)
  (jade-debugger-locals-maybe-refresh)
  (jade-debugger-frames-maybe-refresh))

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
    (jade-debugger-remove-highlights)
    (overlay-put (make-overlay beg end)
                 'face 'jade-highlight-face)))

(defun jade-debugger-remove-highlights ()
  (remove-overlays (point-min) (point-max) 'face 'jade-highlight-face))

(defun jade-debugger-top-frame ()
  "Return the top frame of the current debugging context."
  (car (jade-debugger-frames)))

(defun jade-debugger-step-into ()
  "Request a step into."
  (interactive)
  (jade-debugger-unset-current-buffer)
  (jade-backend-step-into (jade-backend)))

(defun jade-debugger-step-over ()
  "Request a step over."
  (interactive)
  (jade-debugger-unset-current-buffer)
  (jade-backend-step-over (jade-backend)))

(defun jade-debugger-step-out ()
  "Request a step out."
  (interactive)
  (jade-debugger-unset-current-buffer)
  (jade-backend-step-out (jade-backend)))

(defun jade-debugger-resume ()
  (interactive)
  (jade-backend-resume (jade-backend) #'jade-debugger-resumed)
  (jade-debugger-unset-context)
  (let ((locals-buffer (jade-debugger-locals-get-buffer))
        (frames-buffer (jade-debugger-frames-get-buffer)))
    (when locals-buffer
      (kill-buffer locals-buffer))
    (when frames-buffer
      (kill-buffer frames-buffer))
    (if buffer-file-name
        (jade-debugger-unset-current-buffer)
      (kill-buffer))))

(defun jade-debugger-here ()
  (interactive)
  (jade-backend-continue-to-location (jade-backend)
                                     `((scriptId . ,(map-nested-elt (jade-debugger-top-frame)
                                                                    '(location scriptId)))
                                       (lineNumber . ,(1- (line-number-at-pos))))))

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
  (jade-backend-evaluate (jade-backend)
                         expression
                         callback))

(defun jade-debugger-inspect-last-node ()
  "Evaluate and inspect the node before point."
  (interactive)
  (jade-debugger-eval (js2-node-string (jade-interaction-node-before-point))
                      (lambda (result error)
                        (when error
                          (message "JS error: %s" result))
                        (jade-inspector-inspect result))))

;; Debugging context

(defun jade-debugger-setup-context (frames current-frame)
  "Add required debugging information for the current connection.
Put FRAMES and CURRENT-FRAME information as debugging context."
  (map-put jade-connection 'frames frames)
  (map-put jade-connection 'current-frame current-frame))

(defun jade-debugger-set-current-frame (frame)
  "Set FRAME as the current frame."
  ;; when a buffer is already debugging a frame, be sure to clean it first.
  (if-let (old-buf (jade-debugger-get-buffer-create))
      (with-current-buffer old-buf
        (jade-debugger-unset-current-buffer)))
  (map-put jade-connection 'current-frame frame))

(defun jade-debugger-unset-context ()
  "Remove debugging information from the current connection."
  (map-delete jade-connection 'frames)
  (map-delete jade-connection 'current-frame))

(defun jade-debugger-current-frame ()
  "Return the current debugged stack frame."
  (map-elt jade-connection 'current-frame))

(defun jade-debugger-frames ()
  "Return all frames in the current stack."
  (map-elt jade-connection 'frames))

(defun jade-debugger-lookup-file ()
  "Lookup the local file associated with the current connection.
Return nil if no local file can be found."
  (let ((url (jade-backend-get-script-url (jade-backend)
                                          (jade-debugger-current-frame))))
    ;; Make sure we are in the correct directory so that jade can find a ".jade"
    ;; file.
    (with-current-buffer (jade-repl-get-buffer)
      (jade-workspace-lookup-file url))))

(defun jade-debugger-get-current-scopes ()
  "Return the scope of the current stack frame."
  (map-elt (jade-debugger-current-frame) 'scope-chain))

;; TODO: move to backends?
(defun jade-debugger-get-scopes-properties (scopes callback)
  "Request a list of all properties in SCOPES.
CALLBACK is evaluated with the result."
  (seq-do (lambda (scope)
            (jade-debugger-get-scope-properties scope callback))
          ;; ignore the objects attached to global/window
          (seq-remove (lambda (scope)
                        (string= (map-elt scope 'type) "global"))
                      scopes)))

(defun jade-debugger-get-scope-properties (scope callback)
  "Request the properties of SCOPE and evaluate CALLBACK.
CALLBACK is evaluated with two arguments, the properties and SCOPE."
  (jade-backend-get-properties
   (jade-backend)
   (map-nested-elt scope '(object objectid))
   (lambda (properties)
     (funcall callback properties scope))))

(defun jade-debugger-get-buffer-create ()
  "Create a debugger buffer for the current connection and return it.

If a buffer already exists, just return it."
  (let ((buf (if-let ((file (jade-debugger-lookup-file)))
                 (find-file file)
               (get-buffer-create (jade-debugger--buffer-name-no-file)))))
    (jade-debugger-setup-buffer buf)
    buf))

(defun jade-debugger--buffer-name-no-file ()
  "Return the name of a debugger buffer.
This name should used when no local file can be found for a stack
frame."
  "*JS Debugger*")

(defun jade-debugger-setup-buffer (buffer)
  (with-current-buffer buffer
    (unless (or buffer-file-name
                (eq major-mode jade-debugger-major-mode))
      (funcall jade-debugger-major-mode))
    (jade-debugger-mode 1)
    (read-only-mode)))

(defun jade-debugger-unset-current-buffer ()
  "Unset `jade-debugger-mode from the current buffer'."
  (jade-debugger-remove-highlights)
  (when overlay-arrow-position
    (set-marker overlay-arrow-position nil (current-buffer)))
  (jade-debugger-mode -1)
  (read-only-mode -1)
  (jade-debugger-litable-unset-buffer))

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

(provide 'jade-debugger)
;;; jade-debugger.el ends here
