;;; indium-interaction-test.el --- Test for indium-interaction.el  -*- lexical-binding: t; -*-

;; Copyright (C) 2016-2017  Nicolas Petton

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

(require 'ert)
(require 'indium-interaction)

(ert-deftest indium-node-before-point-test ()
  (with-js2-buffer "var foo = 2;\nfoo"
    (should (equal (js2-node-string (indium-interaction-node-before-point))
                   "foo")))
  (with-js2-buffer "var foo = 2;\nfoo + 1;"
    (should (equal (js2-node-string (indium-interaction-node-before-point))
                   "foo + 1;")))
  (with-js2-buffer "var foo = 2;\nfoo + 1;"
    (should (equal (js2-node-string (indium-interaction-node-before-point))
                   "foo + 1;")))
  (with-js2-buffer "var foo = 2; var bar = 3;"
    (should (equal (js2-node-string (indium-interaction-node-before-point))
                   "var bar = 3;")))
  (with-js2-buffer "this.bar(3); this.baz(3);"
    (should (equal (js2-node-string (indium-interaction-node-before-point))
                   "this.baz(3);")))
  (with-js2-buffer "[1,2,\n3,\n4]"
    (should (equal (js2-node-string (indium-interaction-node-before-point))
                   "[1,2,\n3,\n4]")))
  (with-js2-buffer "foo(a,\nb,\nc);"
    (should (equal (js2-node-string (indium-interaction-node-before-point))
                   "foo(a,\nb,\nc);")))
  (with-js2-buffer "foo({\na: 1,\nb: 2,\nc: 3\n});"
    (should (equal (js2-node-string (indium-interaction-node-before-point))
                   "foo({\na: 1,\nb: 2,\nc: 3\n});")))
  (with-js2-buffer "foo({\na: 1,\nb: 2,\nc: 3\n});"
    (goto-char 11)
    (should (equal (js2-node-string (indium-interaction-node-before-point))
                   "1"))
    (goto-char 12)
    (should (equal (js2-node-string (indium-interaction-node-before-point))
                   "1"))
    (goto-char 25)
    (should (equal (js2-node-string (indium-interaction-node-before-point))
                   "{\na: 1,\nb: 2,\nc: 3\n}"))
    (goto-char 26)
    (should (equal (js2-node-string (indium-interaction-node-before-point))
                   "foo({\na: 1,\nb: 2,\nc: 3\n})")))
  (with-js2-buffer "function() { return 1; }"
    (goto-char (point-max))
    (should (equal (js2-node-string (indium-interaction-node-before-point))
                   "function() { return 1; }"))
    (goto-char (1- (point-max)))
    (should (equal (js2-node-string (indium-interaction-node-before-point))
                   "return 1;"))))

(provide 'indium-interaction-test)
;;; indium-interaction-test.el ends here
