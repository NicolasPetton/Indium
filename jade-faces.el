;;; jade-faces.el --- Faces for jade                 -*- lexical-binding: t; -*-

;; Copyright (C) 2016  Nicolas Petton

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

(defgroup jade-faces nil
  "Faces used in jade."
  :prefix "jade-"
  :group 'jade)

(defface jade-keyword-face
  '((t (:inherit font-lock-keyword-face)))
  "Face for the keywords."
  :group 'jade-faces)

(defface jade-button-face
  '((t (:inherit custom-button)))
  "Face for buttons."
  :group 'jade-faces)

(defface jade-header-face
  '((t (:inherit header-line)))
  "Face use in headers."
  :group 'jade-faces)

(defface jade-repl-prompt-face
  '((t (:inherit jade-keyword-face)))
  "Face for the prompt in the REPL buffer."
  :group 'jade-faces)

(defface jade-repl-stdout-face
  '((t (:inherit font-lock-string-face)))
  "Face for the STDOUT output in the REPL buffer."
  :group 'jade-faces)

(defface jade-repl-error-face
  '((t (:inherit font-lock-warning-face)))
  "Face for the error output in the REPL buffer."
  :group 'jade-faces)

(defface jade-link-face
  '((t (:inherit link)))
  "Face used when outputting objects to which we can navigate to."
  :group 'jade-faces)

(defface jade-highlight-face
  '((t (:inherit highlight)))
  "Face used when highlighting regions of a buffer."
  :group 'jade-faces)

(defface jade-frame-url-face
  '((t (:inherit font-lock-comment-face)))
  "Face used to display urls of stack frames."
  :group 'jade-faces)

(provide 'jade-faces)
;;; jade-faces.el ends here
