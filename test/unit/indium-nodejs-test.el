;;; indium-nodejs-test.el --- Unit tests for indium-nodejs.el  -*- lexical-binding: t; -*-

;; Copyright (C) 2017  Nicolas Petton

;; Author: Nicolas Petton <nicolas@petton.fr>
;; Keywords: test

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

;;; Code:

(require 'buttercup)
(require 'indium-nodejs)

(describe "Executing NodeJS processes"
  (it "should set the correct flags when executing nodejs"
    (spy-on 'make-process)
    (spy-on 'switch-to-buffer)
    (spy-on 'process-buffer)

    (spy-on 'indium-nodejs--add-flags)
    (indium-run-node "node foo")
    (expect #'indium-nodejs--add-flags
            :to-have-been-called-with "node foo")))

(describe "Setting NodeJS debug flags"
  (it "should set the `--append' and `--debug-brk' flags"
    (expect (indium-nodejs--add-flags "node")
            :to-equal "node --inspect --debug-brk"))

  (it "should insert the debug flags after the program name and before the arguments"
    (expect (indium-nodejs--add-flags "node app.js")
            :to-equal "node --inspect --debug-brk app.js")))

(describe "Connecting to a NodeJS process"
  (it "should find the websocket URL from the process output"
    (spy-on 'indium-nodejs--connect)
    (let ((output "To start debugging, open the following URL in Chrome:
    chrome-devtools://devtools/bundled/inspector.html?experiments=true&v8only=true&ws=127.0.0.1:9229/43c07a90-1aed-4753-961d-1d449b21e84f"))
      (indium-nodejs--connect-to-process output)
      (expect #'indium-nodejs--connect
              :to-have-been-called-with "127.0.0.1" "9229" "43c07a90-1aed-4753-961d-1d449b21e84f"))))

(describe "Setting up the exec path"
  (it "should setup the exec path before starting a node process"
    (spy-on 'make-process)
    (spy-on 'switch-to-buffer)
    (spy-on 'process-buffer)

    (spy-on 'indium--setup-exec-path)
    (indium-run-node "node foo")
    (expect #'indium--setup-exec-path :to-have-been-called)))

(provide 'indium-nodejs-test)
;;; indium-nodejs-test.el ends here
