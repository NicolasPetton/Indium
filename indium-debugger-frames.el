;;; indium-debugger-frames.el --- List the stack frame  -*- lexical-binding: t; -*-

;; Copyright (C) 2017-2018  Nicolas Petton

;; Author: Nicolas Petton <nicolas@petton.fr>

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

(require 'indium-render)
(require 'indium-script)
(require 'indium-structs)

(declare-function indium-debugger-select-frame "indium-debugger.el")

(defun indium-debugger-stack-frames ()
  "List the stack frames in a separate buffer and switch to it."
  (interactive)
  (let ((buf (indium-debugger-frames-get-buffer-create))
        (inhibit-read-only t))
    (with-current-buffer buf
      (indium-debugger-frames-list (indium-current-connection-frames)
				   (indium-current-connection-current-frame)))
    (pop-to-buffer buf)))

(defun indium-debugger-frames-maybe-refresh ()
  "When a buffer listing the stack frames is open, refresh it."
  (interactive)
  (let ((buf (indium-debugger-frames-get-buffer))
        (inhibit-read-only t))
    (when buf
      (with-current-buffer buf
        (indium-debugger-frames-list (indium-current-connection-frames)
				     (indium-current-connection-current-frame))))))

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
       (indium-location-file (indium-script-original-location
			      (indium-frame-script frame)
			      (indium-frame-location frame)))
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

(provide 'indium-debugger-frames)
;;; indium-debugger-frames.el ends here
