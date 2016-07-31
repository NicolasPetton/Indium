;;; jade.el --- JavaScript Awesome Development Environment  -*- lexical-binding: t; -*-

;; Copyright (C) 2016  Nicolas Petton

;; Author: Nicolas Petton <nicolas@petton.fr>
;; Keywords: tools, javascript
;; Version: 0.10
;; Package-Requires: ((emacs "25") (seq "2.18") (map "1.1") (js2-mode "20140114"))

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

;; Jade connects to a browser tab or nodejs process and provides many features
;; for JavaScript development, including a REPL (with auto completion) & object
;; inspection, an inspector, with history and navigation, and even a stepping
;; Debugger, similar to `edebug`, or `cider`.

;;; Code:

(require 'jade-backend)
(require 'jade-chrome)
(require 'jade-nodejs)

(provide 'jade)
;;; jade.el ends here
