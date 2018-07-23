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

(describe "Reading tab data"
  (it "should be able to parse tab data"
    (let ((data "HTTP/1.1 200 OK

{\"foo\": 1, \"bar\": 2}"))
      (with-temp-buffer
	(insert data)
	(goto-char (point-min))
	(expect (indium-chrome--read-tab-data)
		:to-equal '((foo . 1)
			    (bar . 2))))))

  (it "should return nil when there is no data"
    (with-temp-buffer
      (goto-char (point-min))
      (expect (indium-chrome--read-tab-data)
	      :to-equal nil)))

  (it "should return nil when not getting a 200 response"
        (let ((data "HTTP/1.1 404 NOT-FOUND

{\"foo\": 1, \"bar\": 2}"))
      (with-temp-buffer
	(insert data)
	(goto-char (point-min))
	(expect (indium-chrome--read-tab-data)
		:to-equal nil)))))

(describe "Connecting to a tab"
  (it "should signal an error when the list of tabs is empty"
    (expect (indium-chrome--connect-to-tab nil)
	    :to-throw))

  (it "should automatically connect to the first tab if there is only one"
    (spy-on 'indium-chrome--connect-to-tab-with-url)
    (let ((tabs '(((url . foo)))))
      (indium-chrome--connect-to-tab tabs)
      (expect #'indium-chrome--connect-to-tab-with-url
	      :to-have-been-called-with 'foo tabs))))

(describe "Running Chrome"
  (it "Should start the Chrome process"
    (spy-on 'indium-chrome--find-executable :and-return-value "chrome")
    (spy-on 'make-process)
    (spy-on 'indium-chrome--try-connect)
    (spy-on 'indium-chrome--url :and-return-value "foo.html")
    (spy-on 'indium-chrome--port :and-return-value 9999)
    (indium-launch-chrome)
    (expect #'make-process
	    :to-have-been-called-with
	    :name "indium-chrome-process"
	    :command '("chrome" "--remote-debugging-port=9999" "foo.html")))

  (it "Should try to open connections"
    (spy-on 'indium-chrome--find-executable :and-return-value "chrome")
    (spy-on 'make-process)
    (spy-on 'indium-chrome--try-connect)
    (spy-on 'indium-chrome--url :and-return-value "foo.html")
    (spy-on 'indium-chrome--port :and-return-value 9999)
    (indium-launch-chrome)
    (expect #'indium-chrome--try-connect :to-have-been-called-with 10)))

(describe "Connecting to a Chrome process"
  (it "Should wait between connection retries"
    (spy-on 'sleep-for)
    (spy-on 'indium-chrome--get-tabs-data)
    (indium-chrome--try-connect 1)
    (expect #'sleep-for :to-have-been-called-with 1))

  (it "Should retry if the connection attempt fails"
    (spy-on 'sleep-for)
    (spy-on 'indium-chrome--try-connect :and-call-through)
    (spy-on 'indium-chrome--get-tabs-data
	    :and-call-fake
	    (lambda (host port callback)
	      (funcall callback nil)))
    (indium-chrome--try-connect 1)
    (expect #'indium-chrome--try-connect :to-have-been-called-times 2))

  (it "Should connect to a tab when found"
    (spy-on 'sleep-for)
    (spy-on 'indium-chrome--connect-to-tab)
    (spy-on 'indium-chrome--get-tabs-data
	    :and-call-fake
	    (lambda (host port callback)
	      (funcall callback 'tabs)))
    (indium-chrome--try-connect 1)
    (expect #'indium-chrome--connect-to-tab :to-have-been-called-with 'tabs)))

(describe "Regression test for GH issue #97"
  (it "should not create multiple REPL buffers"
    (let ((buf (indium-repl-get-buffer-create)))
      (expect (indium-repl-get-buffer-create) :to-be buf))))

(provide 'indium-chrome-test)
;;; indium-chrome-test.el ends here
