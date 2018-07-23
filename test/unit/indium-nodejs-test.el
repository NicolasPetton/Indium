;;; indium-nodejs-test.el --- Unit tests for indium-nodejs.el  -*- lexical-binding: t; -*-

;; Copyright (C) 2017-2018  Nicolas Petton

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

    (spy-on 'indium-nodejs--command-with-flags)
    (indium-launch-nodejs)
    (expect #'indium-nodejs--command-with-flags
            :to-have-been-called-with))

  (it "should append extra flags"
    (spy-on #'indium-nodejs--command :and-return-value "node foo")
    (expect (indium-nodejs--command-with-flags)
	    :to-equal "node --inspect foo")
    (spy-on #'indium-nodejs--inspect-brk :and-return-value t)
    (expect (indium-nodejs--command-with-flags)
	    :to-equal "node --inspect-brk foo")
    ;; Regression for GitHub issue #150
    (spy-on #'indium-nodejs--command :and-return-value "ENV_VAR=\"VAL\" node foo")
    (expect (indium-nodejs--command-with-flags)
	    :to-equal "ENV_VAR=\"VAL\" node --inspect-brk foo"))

  (it "should kill the previous connection process when there is one"
    (let ((indium-current-connection (make-indium-connection
				      :process 'first-process)))
      (spy-on #'indium-nodejs--command :and-return-value "node index.js")
      (spy-on 'make-process :and-return-value 'second-process)
      (spy-on 'y-or-n-p :and-return-value t)

      (spy-on 'switch-to-buffer)
      (spy-on 'kill-process)
      (spy-on 'process-buffer)
      (spy-on 'process-status :and-return-value 'run)
      (spy-on 'indium-backend-close-connection)

      (indium-launch-nodejs)

      (expect #'kill-process :to-have-been-called-with 'first-process)
      (expect #'indium-backend-close-connection :to-have-been-called))))

(describe "Connecting to a NodeJS process"
  (it "should find the websocket URL from the process output"
    (spy-on 'indium-nodejs--connect)
    (let ((output "To start debugging, open the following URL in Chrome:
    chrome-devtools://devtools/bundled/inspector.html?experiments=true&v8only=true&ws=127.0.0.1:9229/43c07a90-1aed-4753-961d-1d449b21e84f"))
      (indium-nodejs--connect-to-process 'process output)
      (expect #'indium-nodejs--connect
              :to-have-been-called-with "127.0.0.1" "9229" "43c07a90-1aed-4753-961d-1d449b21e84f" 'process))))

(describe "Connecting to a NodeJS process"
  (it "should connect to process using a host, port and path."
    (spy-on 'indium-v8--open-ws-connection)
    (let ((path "43c07a90-1aed-4753-961d-1d449b21e84f"))
      (indium-nodejs--connect "localhost" 9229 path)
      (expect #'indium-v8--open-ws-connection
              :to-have-been-called-with
	      (format "file://%s" default-directory)
	      "ws://localhost:9229/43c07a90-1aed-4753-961d-1d449b21e84f"
	      nil
	      t))))

(provide 'indium-nodejs-test)
;;; indium-nodejs-test.el ends here
