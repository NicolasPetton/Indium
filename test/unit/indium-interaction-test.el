;;; indium-interaction-test.el --- Test for indium-interaction.el  -*- lexical-binding: t; -*-

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
(require 'assess)

(require 'indium-interaction)
(require 'indium-client)

(describe "Killing previous connections when connecting"
  (it "should kill the previous connection process when there is one"
    (spy-on #'indium-client-process-live-p :and-return-value nil)
    (spy-on #'indium-client-start)
    (spy-on #'indium-maybe-quit)

    (indium-connect)
    (expect #'indium-maybe-quit :to-have-been-called))

  (it "should call `indium-quit' when the user confirms"
    (spy-on #'yes-or-no-p :and-return-value t)
    (spy-on #'indium-client-process-live-p :and-return-value t)
    (spy-on #'indium-quit)
    (indium-maybe-quit)
    (expect #'indium-quit :to-have-been-called))

  (it "should not call `indium-quit' when the user cancels"
    (spy-on #'yes-or-no-p :and-return-value nil)
    (spy-on #'indium-client-process-live-p :and-return-value t)
    (spy-on #'indium-quit)
    (indium-maybe-quit)
    (expect #'indium-quit :not :to-have-been-called)))

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
              :to-equal "return 1;")))

  (it "can find for loop nodes"
    (with-js2-buffer "for (var i = 0; i < 9; i++) {console.log(i);};"
      (goto-char (point-max))
      (expect (js2-node-string (indium-interaction-node-before-point))
              :to-equal "for (var i = 0; i < 9; i++) {console.log(i);}")))

  (it "can find while loop nodes"
    (with-js2-buffer "var i = 9; while (i > 0) {console.log(--i);};"
      (goto-char (point-max))
      (expect (js2-node-string (indium-interaction-node-before-point))
              :to-equal "while (i > 0) {console.log(--i);}")))

  (it "can find do-while loop nodes"
    (with-js2-buffer "var i = 9; do {console.log(--i);} while (i > 0);"
      (goto-char (point-max))
      (expect (js2-node-string (indium-interaction-node-before-point))
              :to-equal "do {console.log(--i);} while (i > 0);")))

  (it "can find for-in loop nodes"
    (with-js2-buffer "for (let k in [3, 2, 1]) {console.log(k);};"
      (goto-char (point-max))
      (expect (js2-node-string (indium-interaction-node-before-point))
              :to-equal "for (let k in [3, 2, 1]) {console.log(k);}")))

  (it "can find for-of loop nodes"
    (with-js2-buffer "for (const x of [1, 2, 3]) {console.log(x);};"
      (goto-char (point-max))
      (expect (js2-node-string (indium-interaction-node-before-point))
              :to-equal "for (const x of [1, 2, 3]) {console.log(x);}"))))

(describe "Adding/removing invalid breakpoints"
  (it "should not try to add duplicate breakpoints"
    (spy-on #'indium-client-add-breakpoint)
    (with-js2-file
      (insert "let a = 2;")
      (indium-add-breakpoint)
      (expect (indium-add-breakpoint) :to-throw 'user-error)))

  (it "should not try to remove non-existant breakpoints"
    (with-js2-file
      (insert "let a = 2;")
      (expect (indium-remove-breakpoint) :to-throw 'user-error)))

  (it "should not try to edit non-existant breakpoints"
    (with-js2-file
      (insert "let a = 2;")
      (expect (indium-edit-breakpoint-condition) :to-throw 'user-error)))

  (it "should not try to add conditional breakpoints twice"
    (spy-on #'indium-client-add-breakpoint)
    (with-js2-file
      (insert "let a = 2;")
      (indium-add-breakpoint)
      (expect (indium-add-conditional-breakpoint "true") :to-throw 'user-error))))

(describe "Adding conditional breakpoints"
  (it "should call `indium-add-breakpoint' with a condition (GH issue #92)"
    (spy-on #'indium-add-breakpoint)
    (with-js2-file
      (insert "let a = 2;")
      (indium-add-conditional-breakpoint "foo")
      (expect #'indium-add-breakpoint :to-have-been-called-with "foo"))))

(describe "Interaction mode"
  (it "should remove breakpoints when killing a buffer"
    (spy-on #'indium-breakpoint-remove-breakpoints-from-current-buffer)
    (spy-on #'indium-client-add-breakpoint)
    (with-js2-file
      (indium-add-breakpoint)
      (kill-buffer)
      (expect #'indium-breakpoint-remove-breakpoints-from-current-buffer :to-have-been-called))))

(provide 'indium-interaction-test)
;;; indium-interaction-test.el ends here
