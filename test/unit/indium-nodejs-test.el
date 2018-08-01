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

;;; Commentary:

;;; Code:

(require 'buttercup)
(require 'indium-nodejs)

(describe "Executing NodeJS processes"
  (it "should set the correct flags when executing nodejs"
    (spy-on #'make-process)
    (spy-on #'set-process-query-on-exit-flag)
    (spy-on #'set-process-sentinel)
    (spy-on #'set-process-filter)
    (spy-on #'switch-to-buffer)
    (spy-on #'process-buffer)

    (spy-on #'indium-nodejs--command-with-flags)

    (with-js2-file
      (indium-launch-nodejs '((command . "node index.js")
			      (inspect-brk . t))))
    (expect #'indium-nodejs--command-with-flags
            :to-have-been-called-with "node index.js" t))

  (it "should append extra flags"
    (expect (indium-nodejs--command-with-flags "node foo" nil)
	    :to-equal "node --inspect foo")
    (expect (indium-nodejs--command-with-flags "node foo" t)
	    :to-equal "node --inspect-brk foo")
    ;; Regression for GitHub issue #150
    (expect (indium-nodejs--command-with-flags "ENV_VAR=\"VAL\" node foo" t)
	    :to-equal "ENV_VAR=\"VAL\" node --inspect-brk foo")))

(provide 'indium-nodejs-test)
;;; indium-nodejs-test.el ends here
