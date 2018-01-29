;;; indium-nodejs-integration-test.el --- Integration tests for indium-nodejs.el  -*- lexical-binding: t; -*-

;; Copyright (C) 2017-2018  Nicolas Petton

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

;; Integration tests for the nodejs connection.  This actually run nodejs, so it
;; has to be installed on the machine.
;;
;; Warning: Before running each test, the current indium-connection is closed!

;;; Code:

(require 'buttercup)
(require 'subr-x)
(require 'cl-lib)

(require 'indium-nodejs)

(describe "NodeJS connection"
  (before-each
   (when-indium-connected
     (indium-quit)))

  (after-each
   (when-indium-connected
     (indium-quit)))

  (it "should be able to start a node process and connect to it"
    (expect indium-current-connection :to-be nil)
    (indium-run-node "node ../fixtures/test.js")
    (sleep-for 2)
    (expect indium-current-connection :not :to-be nil))

  (it "should not try to open a new connection on process output"
    (spy-on 'indium-nodejs--connect-to-process :and-call-through)
    (expect indium-current-connection :to-be nil)
    (indium-run-node "node ../fixtures/test-with-output.js")
    (sleep-for 2)
    (expect #'indium-nodejs--connect-to-process :to-have-been-called-times 1))

  (it "should create a REPL buffer upon connection"
    (expect (get-buffer (indium-repl-buffer-name)) :to-be nil)
    (indium-run-node "node ../fixtures/test.js")
    (sleep-for 2)
    (expect (get-buffer (indium-repl-buffer-name)) :not :to-be nil))

  (it "should run hooks when opening a connection"
    (spy-on 'foo)
    (add-hook 'indium-connection-open-hook #'foo)
    (indium-run-node "node ../fixtures/test.js")
    (sleep-for 2)
    (expect #'foo :to-have-been-called)
    (remove-hook 'indium-connection-open-hook #'foo)))

(provide 'indium-nodejs-integration-test)
;;; indium-nodejs-integration-test.el ends here
