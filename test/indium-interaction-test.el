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

(require 'buttercup)
(require 'indium-interaction)

(describe "Finding the AST node to evaluate"

  (it "can find variable nodes"
    (with-js2-buffer "var foo = 2;\nfoo"
      (expect (js2-node-string (indium-interaction-node-before-point))
        :to-equal "foo")))

  (it "can find expression nodes"
    (with-js2-buffer "var foo = 2;\nfoo + 1;"
      (expect (js2-node-string (indium-interaction-node-before-point))
        :to-equal "foo + 1;")))

  (it "can find assignment nodes"
    (with-js2-buffer "var foo = 2; var bar = 3;"
      (expect (js2-node-string (indium-interaction-node-before-point))
        :to-equal "var bar = 3;")))

  (it "can find function call nodes"
    (with-js2-buffer "this.bar(3); this.baz(3);"
      (expect (js2-node-string (indium-interaction-node-before-point))
        :to-equal "this.baz(3);")))

  (it "can find array literal nodes"
    (with-js2-buffer "[1,2,\n3,\n4]"
      (expect (js2-node-string (indium-interaction-node-before-point))
        :to-equal "[1,2,\n3,\n4]")))

  (it "can find function calls on multiple lines"
    (with-js2-buffer "foo(a,\nb,\nc);"
      (expect (js2-node-string (indium-interaction-node-before-point))
        :to-equal "foo(a,\nb,\nc);")))

  (it "can find function calls with object literal as parameter"
    (with-js2-buffer "foo({\na: 1,\nb: 2,\nc: 3\n});"
      (expect (js2-node-string (indium-interaction-node-before-point))
        :to-equal "foo({\na: 1,\nb: 2,\nc: 3\n});")))

  (it "can find sub nodes"
    (with-js2-buffer "foo({\na: 1,\nb: 2,\nc: 3\n});"
      (goto-char 11)
      (expect (js2-node-string (indium-interaction-node-before-point))
        :to-equal "1")

     (goto-char 12)
     (expect (js2-node-string (indium-interaction-node-before-point))
       :to-equal "1")

     (goto-char 25)
     (expect (js2-node-string (indium-interaction-node-before-point))
       :to-equal "{\na: 1,\nb: 2,\nc: 3\n}")

     (goto-char 26)
     (expect (js2-node-string (indium-interaction-node-before-point))
       :to-equal "foo({\na: 1,\nb: 2,\nc: 3\n})")))

  (it "can find function definitions"
    (with-js2-buffer "function() { return 1; }"
     (goto-char (point-max))
     (expect (js2-node-string (indium-interaction-node-before-point))
       :to-equal "function() { return 1; }")

     (goto-char (1- (point-max)))
     (expect (js2-node-string (indium-interaction-node-before-point))
       :to-equal "return 1;"))))

(provide 'indium-interaction-test)
;;; indium-interaction-test.el ends here
