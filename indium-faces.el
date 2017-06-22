;;; indium-faces.el --- Faces for indium                 -*- lexical-binding: t; -*-

;; Copyright (C) 2016-2017  Nicolas Petton

;; Author: Nicolas Petton <nicolas@petton.fr>
;; Keywords: faces

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

(require 'cus-edit)

(defgroup indium-faces nil
  "Faces used in indium."
  :prefix "indium-"
  :group 'indium)

(defface indium-keyword-face
  '((t (:inherit font-lock-keyword-face)))
  "Face for the keywords."
  :group 'indium-faces)

(defface indium-button-face
  '((t (:inherit custom-button)))
  "Face for buttons."
  :group 'indium-faces)

(defface indium-header-face
  '((t (:inherit header-line)))
  "Face use in headers."
  :group 'indium-faces)

(defface indium-repl-prompt-face
  '((t (:inherit indium-keyword-face)))
  "Face for the prompt in the REPL buffer."
  :group 'indium-faces)

(defface indium-repl-stdout-face
  '((t (:inherit font-lock-string-face)))
  "Face for the STDOUT output in the REPL buffer."
  :group 'indium-faces)

(defface indium-repl-error-face
  '((t (:inherit font-lock-warning-face)))
  "Face for the error output in the REPL buffer."
  :group 'indium-faces)

(defface indium-link-face
  '((t (:inherit link)))
  "Face used when outputting objects to which we can navigate to."
  :group 'indium-faces)

(defface indium-highlight-face
  '((t (:inherit highlight)))
  "Face used when highlighting regions of a buffer."
  :group 'indium-faces)

(defface indium-litable-face
  '((t (:inherit font-lock-comment-face :slant italic)))
  "Face used to display inline values in debuggers."
  :group 'indium-faces)

(defface indium-frame-url-face
  '((t (:inherit font-lock-comment-face)))
  "Face used to display urls of stack frames."
  :group 'indium-faces)

(defface indium-breakpoint-face
  '((t (:inherit fringe :foreground "#ff6c6b")))
  "Face used for breakpoints."
  :group 'indium-faces)

(provide 'indium-faces)
;;; indium-faces.el ends here
