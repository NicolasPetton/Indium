;;; indium-seq-fix.el --- Patches for seq.el         -*- lexical-binding: t; -*-

;; Copyright (C) 2017  Nicolas Petton

;; Author: Nicolas Petton <nicolas@petton.fr>
;; Keywords:

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

;; HACK: In Emacs 25.1, an older version of seq.el is provided, which can be
;; loaded before indium or even package.el.  If this happens, the feature `seq'
;; being already provided, the correct version of seq.el won't get loaded.
(require 'seq)
(unless (fboundp 'seq-map-indexed)
  (defun seq-map-indexed (function sequence)
    "Return the result of applying FUNCTION to each element of SEQUENCE.
Unlike `seq-map', FUNCTION takes two arguments: the element of
the sequence, and its index within the sequence."
    (let ((index 0))
      (seq-map (lambda (elt)
                 (prog1
                     (funcall function elt index)
                   (setq index (1+ index))))
               sequence))))

(provide 'indium-seq-fix)
;;; indium-seq-fix.el ends here
