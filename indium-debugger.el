;;; indium-debugger.el --- Indium debugger               -*- lexical-binding: t; -*-

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
(require 'indium-inspector)
(require 'indium-repl)
(require 'indium-interaction)
(require 'indium-render)
(require 'indium-workspace)
(require 'indium-debugger-frames)
(require 'indium-debugger-locals)
(require 'indium-debugger-litable)

(defgroup indium-debugger nil
  "JavaScript debugger"
  :prefix "indium-debugger-"
  :group 'indium)

(defcustom indium-debugger-major-mode
  #'js2-mode
  "Major mode used in debugger buffers."
  :group 'indium-debugger
  :type 'function)

(defvar indium-debugger-buffer nil "Buffer used for debugging JavaScript sources.")

(defvar indium-debugger-message nil "Message to be displayed in the echo area.")
(make-local-variable 'indium-debugger-message)

(defconst indium-debugger-fringe-arrow-string
  #("." 0 1 (display (left-fringe right-triangle)))
  "Used as an overlay's before-string prop to place a fringe arrow.")

(declare 'indium-backend-debugger-get-script-source)

(defun indium-debugger-paused (frames &optional reason)
  (indium-debugger-setup-context frames (car frames))
  (indium-debugger-select-frame (car frames))
  (indium-debugger-show-help-message reason))

(defun indium-debugger-resumed (&rest _args)
  (message "Execution resumed")
  (indium-debugger-unset-context)
  (seq-doseq (buf (seq-filter (lambda (buf)
                                (with-current-buffer buf
                                  indium-debugger-mode))
                              (buffer-list)))
    (with-current-buffer buf
      (set-marker overlay-arrow-position nil (current-buffer))
      (indium-debugger-remove-highlights)
      (indium-debugger-litable-unset-buffer))))

(defun indium-debugger-next-frame ()
  "Jump to the next frame in the frame stack."
  (interactive)
  (indium-debugger--jump-to-frame 'forward))

(defun indium-debugger-previous-frame ()
  "Jump to the previous frame in the frame stack."
  (interactive)
  (indium-debugger--jump-to-frame 'backward))

(defun indium-debugger--jump-to-frame (direction)
  "Jump to the next frame in DIRECTION.
DIRECTION is `forward' or `backward' (in the frame list)."
  (let* ((current-position (seq-position (indium-debugger-frames) (indium-debugger-current-frame)))
         (step (pcase direction
                 (`forward -1)
                 (`backward 1)))
         (position (+ current-position step)))
    (when (>= position (seq-length (indium-debugger-frames)))
      (user-error "End of frames"))
    (when (< position 0)
      (user-error "Beginning of frames"))
    (indium-debugger-select-frame (seq-elt (indium-debugger-frames) position))))

(defun indium-debugger-select-frame (frame)
  "Make FRAME the current debugged stach frame.
Switch to the buffer for FRAME.

Try to find the file locally first using Indium worskspaces.  If a
local file cannot be found, get the remote source and open a new
buffer visiting it."
  (indium-debugger-set-current-frame frame)
  (indium-debugger-litable-setup-buffer)
  (switch-to-buffer (indium-debugger-get-buffer-create))
  (if buffer-file-name
      (indium-debugger-setup-buffer-with-file)
    (indium-backend-get-script-source
       (indium-backend)
       frame
       (lambda (source)
         (indium-debugger-setup-buffer-no-file
          (map-nested-elt source '(result scriptSource)))))))

(defun indium-debugger-setup-buffer-with-file ()
  "Setup the current buffer for debugging."
  (when (buffer-modified-p)
    (revert-buffer nil nil t))
  (indium-debugger-position-buffer))

(defun indium-debugger-setup-buffer-no-file (source)
  "Setup the current buffer with the frame source SOURCE."
  (unless (string= (buffer-substring-no-properties (point-min) (point-max))
                   source)
    (let ((inhibit-read-only t))
      (erase-buffer)
      (insert source)))
  (indium-debugger-position-buffer))

(defun indium-debugger-position-buffer ()
  (let* ((frame (indium-debugger-current-frame))
         (location (map-elt frame 'location))
         (line (map-elt location 'lineNumber))
         (column (map-elt location 'columnNumber)))
    (goto-char (point-min))
    (forward-line line)
    (forward-char column))
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
  (let ((pos (line-beginning-position)))
    (setq overlay-arrow-string "=>")
    (setq overlay-arrow-position (make-marker))
    (set-marker overlay-arrow-position pos (current-buffer))))

(defun indium-debugger-highlight-node ()
  (let ((beg (point))
        (end (line-end-position)))
    (indium-debugger-remove-highlights)
    (overlay-put (make-overlay beg end)
                 'face 'indium-highlight-face)))

(defun indium-debugger-remove-highlights ()
  (remove-overlays (point-min) (point-max) 'face 'indium-highlight-face))

(defun indium-debugger-top-frame ()
  "Return the top frame of the current debugging context."
  (car (indium-debugger-frames)))

(defun indium-debugger-step-into ()
  "Request a step into."
  (interactive)
  (indium-debugger-unset-current-buffer)
  (indium-backend-step-into (indium-backend)))

(defun indium-debugger-step-over ()
  "Request a step over."
  (interactive)
  (indium-debugger-unset-current-buffer)
  (indium-backend-step-over (indium-backend)))

(defun indium-debugger-step-out ()
  "Request a step out."
  (interactive)
  (indium-debugger-unset-current-buffer)
  (indium-backend-step-out (indium-backend)))

(defun indium-debugger-resume ()
  (interactive)
  (indium-backend-resume (indium-backend) #'indium-debugger-resumed)
  (let ((locals-buffer (indium-debugger-locals-get-buffer))
        (frames-buffer (indium-debugger-frames-get-buffer)))
    (when locals-buffer
      (kill-buffer locals-buffer))
    (when frames-buffer
      (kill-buffer frames-buffer))
    (if buffer-file-name
        (indium-debugger-unset-current-buffer)
      (kill-buffer))))

(defun indium-debugger-here ()
  (interactive)
  (indium-backend-continue-to-location (indium-backend)
                                     `((scriptId . ,(map-nested-elt (indium-debugger-top-frame)
                                                                    '(location scriptId)))
                                       (lineNumber . ,(1- (line-number-at-pos))))))

(defun indium-debugger-evaluate (expression)
  "Prompt for EXPRESSION to be evaluated.
Evaluation happens in the context of the current call frame."
  (interactive "sEvaluate on frame: ")
  (indium-backend-evaluate (indium-backend)
                         expression
                         (lambda (value _error)
                           (message "%s" (indium-render-value-to-string value)))))

;; Debugging context

(defun indium-debugger-setup-context (frames current-frame)
  "Add required debugging information for the current connection.
Put FRAMES and CURRENT-FRAME information as debugging context."
  (map-put indium-connection 'frames frames)
  (map-put indium-connection 'current-frame current-frame))

(defun indium-debugger-set-current-frame (frame)
  "Set FRAME as the current frame."
  ;; when a buffer is already debugging a frame, be sure to clean it first.
  (if-let (old-buf (indium-debugger-get-buffer-create))
      (with-current-buffer old-buf
        (indium-debugger-unset-current-buffer)))
  (map-put indium-connection 'current-frame frame))

(defun indium-debugger-unset-context ()
  "Remove debugging information from the current connection."
  (map-delete indium-connection 'frames)
  (map-delete indium-connection 'current-frame))

(defun indium-debugger-current-frame ()
  "Return the current debugged stack frame."
  (map-elt indium-connection 'current-frame))

(defun indium-debugger-frames ()
  "Return all frames in the current stack."
  (map-elt indium-connection 'frames))

(defun indium-debugger-lookup-file ()
  "Lookup the local file associated with the current connection.
Return nil if no local file can be found."
  (let ((url (indium-backend-get-script-url (indium-backend)
                                            (indium-debugger-current-frame))))
    ;; Make sure we are in the correct directory so that indium can find a ".indium"
    ;; file.
    (with-current-buffer (indium-repl-get-buffer)
      (indium-workspace-lookup-file url))))

(defun indium-debugger-get-current-scopes ()
  "Return the scope of the current stack frame."
  (map-elt (indium-debugger-current-frame) 'scope-chain))

;; TODO: move to backends?
(defun indium-debugger-get-scopes-properties (scopes callback)
  "Request a list of all properties in SCOPES.
CALLBACK is evaluated with the result."
  (seq-do (lambda (scope)
            (indium-debugger-get-scope-properties scope callback))
          ;; ignore the objects attached to global/window
          (seq-remove (lambda (scope)
                        (string= (map-elt scope 'type) "global"))
                      scopes)))

(defun indium-debugger-get-scope-properties (scope callback)
  "Request the properties of SCOPE and evaluate CALLBACK.
CALLBACK is evaluated with two arguments, the properties and SCOPE."
  (indium-backend-get-properties
   (indium-backend)
   (map-nested-elt scope '(object objectid))
   (lambda (properties)
     (funcall callback properties scope))))

(defun indium-debugger-get-buffer-create ()
  "Create a debugger buffer for the current connection and return it.

If a buffer already exists, just return it."
  (let ((buf (if-let ((file (indium-debugger-lookup-file)))
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
  (with-current-buffer buffer
    (unless (or buffer-file-name
                (eq major-mode indium-debugger-major-mode))
      (funcall indium-debugger-major-mode))
    (indium-debugger-mode 1)
    (when (and (eq major-mode 'js2-mode)
               js2-mode-buffer-dirty-p)
      (js2-parse))
    (read-only-mode)))

(defun indium-debugger-unset-current-buffer ()
  "Unset `indium-debugger-mode from the current buffer'."
  (indium-debugger-remove-highlights)
  (when overlay-arrow-position
    (set-marker overlay-arrow-position nil (current-buffer)))
  (indium-debugger-mode -1)
  (read-only-mode -1)
  (indium-debugger-litable-unset-buffer))

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
    map))

(define-minor-mode indium-debugger-mode
  "Minor mode for debugging JS scripts.

\\{indium-debugger-mode-map}"
  :group 'indium
  :lighter " JS-debug"
  :keymap indium-debugger-mode-map
  (if indium-debugger-mode
      (progn
        (unless indium-interaction-mode
          (indium-interaction-mode))
        (add-hook 'pre-command-hook #'indium-debugger-refresh-echo-area nil t))
    (remove-hook 'pre-command-hook #'indium-debugger-refresh-echo-area t)))

(provide 'indium-debugger)
;;; indium-debugger.el ends here
