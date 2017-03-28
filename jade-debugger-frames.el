;;; jade-debugger-frames.el --- List the stack frame  -*- lexical-binding: t; -*-

;; Copyright (C) 2017  Nicolas Petton

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

(require 'jade-render)

(declare 'jade-debugger-frames)
(declare 'jade-debugger-current-frame)
(declare 'jade-debugger-select-frame)

(defun jade-debugger-stack-frames ()
  "List the stack frames in a separate buffer and switch to it."
  (interactive)
  (let ((buf (jade-debugger-frames-get-buffer-create))
        (frames (jade-debugger-frames))
        (current-frame (jade-debugger-current-frame))
        (inhibit-read-only t))
    (with-current-buffer buf
     (jade-debugger-frames-list frames current-frame))
    (switch-to-buffer buf)))

(defun jade-debugger-frames-maybe-refresh ()
  "When a buffer listing the stack frames is open, refresh it."
  (interactive)
  (let ((buf (jade-debugger-frames-get-buffer))
        (frames (jade-debugger-frames))
        (current-frame (jade-debugger-current-frame))
        (inhibit-read-only t))
    (when buf
      (with-current-buffer buf
        (jade-debugger-frames-list frames current-frame)))))

(defun jade-debugger-frames-list (frames &optional current-frame)
  "Render the list of stack frames FRAMES.
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
  (jade-debugger-select-frame frame))

(defun jade-debugger-frames-next-frame ()
  "Go to the next frame in the stack."
  (interactive)
  (jade-debugger-frames-goto-next 'next))

(defun jade-debugger-frames-previous-frame ()
  "Go to the previos frame in the stack."
  (interactive)
    (jade-debugger-frames-goto-next 'previous))

(defun jade-debugger-frames-goto-next (direction)
  "Go to the next frame in DIRECTION."
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
  "Setup BUFFER for the Jade connection CONNECTION."
  (with-current-buffer buffer
    (jade-debugger-frames-mode)
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
  (font-lock-ensure)
  (read-only-mode))

(provide 'jade-debugger-frames)
;;; jade-debugger-frames.el ends here
