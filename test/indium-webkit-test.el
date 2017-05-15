;;; indium-webkit-test.el --- Tests for indum-webkit.el  -*- lexical-binding: t; -*-

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

;; Tests for indium-webkit.el

;;; Code:

(require 'buttercup)
(require 'indium-webkit)

(describe "Webkit backend result description string"
  (it "can render booleans (GitHub issue #52)"
    (expect (indium-webkit--description '((type . "boolean") (value . "true")))
      :to-equal "true")
    (expect (indium-webkit--description '((type . "boolean") (value . "false")))
      :to-equal "false")))

(provide 'indium-webkit-test)
;;; indium-webkit-test.el ends here
