;;; jade-scratch.el --- Scratch buffer for JS evaluation  -*- lexical-binding: t; -*-

;; Copyright (C) 2016  Nicolas Petton

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

;; Open a scratch buffer with `jade-scratch'.

;;; Code:

(require 'jade-interaction)
(require 'jade-repl)

(defun jade-scratch ()
  "Pop to the scratch buffer.
If no scratch buffer exists for the current connection, create
one first."
  (interactive)
  (pop-to-buffer (jade-scratch-get-buffer-create)))

(defun jade-scratch-get-buffer-create ()
  "Return a scratch buffer for the current connection.
If no buffer exists, create one.

If there is no current connection, throw an error."
  (unless jade-connection
    (user-error "No current connection"))
  (let* ((url (map-elt jade-connection 'url))
         (bufname (jade-scratch-buffer-name url))
         (buf (get-buffer bufname)))
    (unless buf
      (setq buf (get-buffer-create bufname))
      (jade-scratch-setup-buffer buf jade-connection))
    buf))

(defun jade-scratch-buffer-name (url)
  "Return the scratch buffer name for URL."
  (format "*JS scratch %s*" url))

(defun jade-scratch-setup-buffer (buffer connection)
  "Setup the scratch BUFFER for  CONNECTION."
  (with-current-buffer buffer
    ;; TODO: enable completion like in the REPL
    (js2-mode)
    (jade-interaction-mode)
    (setq-local jade-connection connection)
    (jade-scratch-insert-welcome-message)))

(defun jade-scratch-insert-welcome-message ()
  "Insert a welcome message to help use the scratch buffer."
  (insert "// This buffer is for JavaScript evaluation.
// Press C-x C-e to evaluate the last expression.
// Press C-c M-i to inspect the last expression.\n\n"))

(provide 'jade-scratch)
;;; jade-scratch.el ends here
