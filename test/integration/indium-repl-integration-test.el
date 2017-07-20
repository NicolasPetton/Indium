;;; indium-repl-integration-test.el --- Integration tests for indium-repl.el  -*- lexical-binding: t; -*-

;; Copyright (C) 2017  Nicolas Petton

;; Author: Nicolas Petton <nicolas@petton.fr>
;; Keywords: test, integration

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

;; Integration tests for the repl buffer.  This actually runs nodejs, so it
;; has to be installed on the machine.
;;
;; Warning: Before running each test, the current indium-connection is closed!

;;; Code:

(require 'buttercup)
(require 'subr-x)
(require 'cl-lib)

(require 'indium-repl)
(require 'indium-nodejs)

(describe "Repl output"
  (it "should display a prompt"
    (with-repl-buffer
      (expect (buffer-string) :to-match "js> $")))

  (it "should display a welcome message"
    (with-repl-buffer
      (expect (buffer-string) :to-match "Welcome to Indium")))

  (it "should be able to clear all output"
    (with-repl-buffer
      (press "C-c C-o")
      (expect (buffer-string) :to-match "^js> $")))

  (it "should be able eval and print results"
    (with-repl-buffer
      (repl-eval "true")
      (expect (buffer-string) :to-match "js> true\ntrue\njs> $")))

  (it "should be able to inspect objects"
    (with-repl-buffer
      (insert "console")
      (expect (indium-inspector-get-buffer) :to-be nil)
      (press-and-sleep-for "C-c M-i" 2)
      (expect (indium-inspector-get-buffer) :not :to-be nil))))

(provide 'indium-repl-integration-test)
;;; indium-repl-integration-test.el ends here
