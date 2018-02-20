;;; indium.el --- JavaScript Awesome Development Environment  -*- lexical-binding: t; -*-

;; Copyright (C) 2016-2018  Nicolas Petton

;; Author: Nicolas Petton <nicolas@petton.fr>
;; URL: https://github.com/NicolasPetton/indium
;; Keywords: tools, javascript
;; Version: 1.2.0
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

(require 'cl-lib)
(require 'indium-chrome)
(require 'indium-nodejs)
(require 'indium-scratch)
(require 'indium-list-scripts)

(provide 'indium)
;;; indium.el ends here
