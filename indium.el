;;; indium.el --- JavaScript Awesome Development Environment  -*- lexical-binding: t; -*-

;; Copyright (C) 2016-2017  Nicolas Petton

;; Author: Nicolas Petton <nicolas@petton.fr>
;; URL: https://github.com/NicolasPetton/indium
;; Keywords: tools, javascript
;; Version: 0.6.1
;; Package-Requires: ((emacs "25") (seq "2.16") (js2-mode "20140114") (company "0.9.0") (websocket "1.6"))

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

;; Indium connects to a browser tab or nodejs process and provides many features
;; for JavaScript development, including a REPL (with auto completion) & object
;; inspection, an inspector, with history and navigation, and even a stepping
;; Debugger, similar to `edebug`, or `cider`.

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

(require 'indium-backend)
(require 'indium-chrome)
(require 'indium-nodejs)
(require 'indium-scratch)

(provide 'indium)
;;; indium.el ends here
