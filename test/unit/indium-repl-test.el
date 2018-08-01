;;; indium-repl-test.el --- Unit tests for indium-repl.el  -*- lexical-binding: t; -*-

;; Copyright (C) 2017-2018  Nicolas Petton

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

;; Unit tests for indium-repl.el

;;; Code:

(require 'indium-repl)
(require 'indium-interaction)

(require 'buttercup)

(describe "Switching from and to the REPL buffer"
  (it "should throw an error when not connected"
    (spy-on #'indium-client-process-live-p :and-return-value nil)
    (expect (indium-switch-to-repl-buffer) :to-throw 'user-error))

  (it "should be able to switch to the REPL buffer when connected"
    (spy-on #'indium-client-process-live-p :and-return-value t)
    (spy-on #'pop-to-buffer)
    (indium-switch-to-repl-buffer)
    (expect #'pop-to-buffer :to-have-been-called))

  (it "should be able to switch back from the REPL buffer"
    (spy-on #'indium-client-process-live-p :and-return-value t)
    (spy-on 'pop-to-buffer)
    (let ((indium-repl-switch-from-buffer 'from))
      (indium-repl-pop-buffer)
      (expect #'pop-to-buffer :to-have-been-called-with 'from t))))

(describe "Regression test for GH issue #97"
  (it "should not create multiple REPL buffers"
    (let ((buf (indium-repl-get-buffer-create)))
      (expect (indium-repl-get-buffer-create) :to-be buf))))

(provide 'indium-repl-test)
;;; indium-repl-test.el ends here
