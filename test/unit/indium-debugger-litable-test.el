;;; indium-debugger-litable-test.el --- Test for indium-debugger-litable.el  -*- lexical-binding: t; -*-

;; Copyright (C) 2016-2018  Nicolas Petton

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

;;; Code:

(require 'buttercup)

(require 'indium-debugger-litable)

(describe "Debugger litable (inline values)"
  (before-each
    (spy-on 'window-width :and-return-value 50))

  (it "should truncate long strings"
    (let ((buffer-contents "let foo = 1;")
          (short-string "short")
          (long-string "very very very long string that overflows"))
      (with-temp-buffer
        (insert buffer-contents)
        (expect (indium-debugger-litable--overlay-string short-string)
          :to-equal short-string)
        (expect (indium-debugger-litable--overlay-string long-string)
          :to-equal "very very very long string that ov...")))))

(provide 'indium-debugger-litable-test)
;;; indium-debugger-litable-test.el ends here
