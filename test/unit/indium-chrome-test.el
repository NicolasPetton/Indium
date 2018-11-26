;;; indium-chrome-test.el --- tests for indium-chrome.el  -*- lexical-binding: t; -*-

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

;;

;;; Code:

(require 'buttercup)
(require 'indium-chrome)
(require 'indium-client)
(require 'seq)

(describe "Chrome executable"
  (it "Should try to find the executable"
    (spy-on 'executable-find :and-return-value 'foo)
    (expect (indium-chrome--find-executable) :to-be 'foo)
    (expect #'executable-find :to-have-been-called-with indium-chrome-executable))

  (it "Should return the default executable based on the system"
    (let ((system-type "gnu/linux"))
      (expect (indium-chrome--default-executable)
	      :to-equal "chromium"))
    (let ((system-type "darwin"))
      (expect (indium-chrome--default-executable)
	      :to-equal "/Applications/Google Chrome.app/Contents/MacOS/Google Chrome"))
    (let ((system-type "windows-nt"))
      (expect (indium-chrome--default-executable)
	      :to-equal "C:/Program Files (x86)/Google/Chrome/Application/chrome.exe"))))

(describe "Running Chrome"
  (it "Should start the Chrome process"
    (let ((conf '((url . "http://localhost:9999")
		  (name . "Web project")
		  (type . "chrome")
		  (projectFile . "/foo/bar/.indium.json")
		  (port . "9223")))
	  (indium-chrome-use-temporary-profile nil))
      (spy-on 'indium-chrome--find-executable :and-return-value "chrome")
      (spy-on 'make-process)
      (spy-on 'indium-client-connect)
      (indium-launch-chrome conf)
      (expect #'make-process
	      :to-have-been-called-with
	      :name "indium-chrome-process"
	      :command '("chrome" "--remote-debugging-port=9223" "" "http://localhost:9999"))))

  (it "Should connect to the chrome process"
    (let ((conf '((url . "http://localhost:9999")
		  (name . "Web project")
		  (type . "chrome")
		  (projectFile . "/foo/bar/.indium.json")
		  (port . "9999"))))
      (spy-on 'indium-chrome--find-executable :and-return-value "chrome")
      (spy-on 'make-process)
      (spy-on 'indium-client-connect)
      (indium-launch-chrome conf)
      (expect #'indium-client-connect :to-have-been-called)))

  (it "Should make a temporary profile"
    (spy-on 'indium-chrome--find-executable :and-return-value "chrome")
    (let* ((indium-chrome-use-temporary-profile t)
	   (port 9229)
	   (url "http://localhost:3000")
	   (command (indium-chrome--command port url)))
      (expect (length command) :to-be 4)
      (expect (seq-elt command 0) :to-equal (indium-chrome--find-executable))
      (expect (seq-elt command 1) :to-equal "--remote-debugging-port=9229")
      (expect (seq-elt command 2) :to-match "--user-data-dir=/tmp/.*")
      (expect (seq-elt command 3) :to-equal url))))

(provide 'indium-chrome-test)
;;; indium-chrome-test.el ends here
