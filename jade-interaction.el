;;; jade-interaction.el --- Interaction functions for jade.el  -*- lexical-binding: t; -*-

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

;;; Code:

(require 'js2-mode)
(require 'map)
(require 'seq)
(require 'jade-backend)
(require 'jade-inspector)
(require 'jade-render)
(require 'jade-repl)

(defun jade-eval (string &optional callback)
  "Evaluate STRING on the current backend.
When CALLBACK is non-nil, evaluate CALLBACK with the result.

When called interactively, prompt the user for the string to be
evaluated."
  (interactive "sEvaluate JavaScript: ")
  (jade-backend-evaluate (jade-backend) string callback))

(defun jade-eval-buffer ()
  "Evaluate the accessible portion of current buffer."
  (interactive)
  (jade-interaction--ensure-connection)
  (jade-eval (buffer-string)))

(defun jade-reload-page ()
  "Reload current page."
  (interactive)
  (jade-interaction--ensure-connection)
  (jade-eval "document.location.reload(true)"))

(defvar jade-script-url nil)
(defvar jade-script-bundle-path nil)

(defun jade-set-script-source (&optional url bundle-path)
  "Set script source for URL.
Optionally take compiled source from BUNDLE-PATH."
  (interactive)
  (jade-interaction--ensure-connection)
  (if (and jade-script-url jade-script-bundle-path)
      (setq url jade-script-url
            bundle-path jade-script-bundle-path)
    (let ((scripts (hash-table-keys jade-webkit-source-maps)))
      (setq url (seq-find (lambda (script)
                            (string= (buffer-name) (file-name-nondirectory script)))
                          scripts))
      (unless url
        (setq url (completing-read "Script: " scripts))
        (setq bundle-path (read-file-name "Bundle file name: ")))))
  (let ((script-id (map-nested-elt jade-webkit-source-maps `(,url script-id))))
    (unless script-id
      (user-error "No script found for url: %s" url))
    (setq jade-script-url url)
    (setq jade-script-bundle-path bundle-path)
    (jade-backend-set-script-source (jade-backend)
                                    script-id
                                    (if bundle-path
                                        (with-temp-buffer
                                          (insert-file-contents bundle-path)
                                          (buffer-string))
                                      (buffer-string))
                                    (lambda (response)
                                      (jade-eval (format "window.dispatchEvent(new CustomEvent('change', {detail: {filename: '%s'}}))" (file-name-nondirectory bundle-path)))
                                      (message "Source set %s." response)))))

(defun jade-eval-last-node (arg)
  "Evaluate the node before point; print in the echo area.
This is similar to `eval-last-sexp', but for JavaScript buffers.

Interactively, with a prefix argument ARG, print output into
current buffer."
  (interactive "P")
  (jade-interaction--ensure-connection)
  (jade-eval (js2-node-string (jade-interaction-node-before-point))
             (lambda (value _error)
               (let ((description (jade-description-string value)))
                 (if arg
                     (save-excursion
                       (insert description))
                   (message description))))))

(defun jade-inspect-last-node ()
  "Evaluate and inspect the node before point."
  (interactive)
  (jade-interaction--ensure-connection)
  (jade-eval (js2-node-string (jade-interaction-node-before-point))
             (lambda (result _error)
               (jade-inspector-inspect result))))

(defun jade-interaction-node-before-point ()
  "Return the node before point to be evaluated."
  (save-excursion
    (forward-comment -1)
    (while (looking-back "[:,]" nil)
      (backward-char 1))
    (backward-char 1)
    (let* ((node (js2-node-at-point))
           (parent (js2-node-parent node)))
      ;; Heuristics for finding the node to evaluate: if the parent of the node
      ;; before point is a prop-get node (i.e. foo.bar) and if it starts before
      ;; the current node, meaning that the point is on the node following the
      ;; parent, then evaluate the content of the parent node:
      ;;
      ;; (underscore represents the point)
      ;; foo.ba_r // => evaluate foo.bar
      ;; foo_.bar // => evaluate foo
      ;; foo.bar.baz_() // => evaluate foo.bar.baz
      ;; foo.bar.baz()_ // => evaluate foo.bar.baz()
      (while (and (js2-prop-get-node-p parent)
                  (< (js2-node-abs-pos parent)
                     (js2-node-abs-pos node)))
        (setq node parent))
      node)))

(defun jade-interaction--ensure-connection ()
  "Set a connection if no connection is set for the current buffer.
If the current buffer has no associated `jade-connection', prompt
the user for one of the open connections if many of them are
open, and set it in the current buffer."
  (unless jade-connection
    (setq-local jade-connection
                (if (= 1 (seq-length jade-connections))
                    (seq-elt jade-connections 0)
                  (jade-interaction--read-connection)))))

(defun jade-interaction--read-connection ()
  "Read a connection from the minibuffer, with completion."
  (let ((url (completing-read "Choose a connection: "
                              (seq-map (lambda (conn)
                                         (map-elt conn 'url))
                                       jade-connections))))
    (seq-find (lambda (conn)
                (string= (map-elt conn 'url)
                         url))
              jade-connections)))

(defvar jade-interaction-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-x C-e") #'jade-eval-last-node)
    (define-key map (kbd "C-c M-i") #'jade-inspect-last-node)
    (define-key map (kbd "C-c C-z") #'jade-switch-to-repl-buffer)
    (define-key map (kbd "C-c C-k") #'jade-set-script-source)
    (define-key map (kbd "C-c C-j") #'jade-reload-page)
    map))

(define-minor-mode jade-interaction-mode
  "Mode for JavaScript evalution.

\\{jade-interaction-mode-map}"
  :lighter " js-interaction"
  :keymap jade-interaction-mode-map)

(provide 'jade-interaction)
;;; jade-interaction.el ends here
