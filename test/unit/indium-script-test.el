;;; indium-script-test.el --- Unit tests for indium-script.el  -*- lexical-binding: t; -*-

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

;;; Commentary:

;;; Code:

(require 'buttercup)
(require 'assess)
(require 'indium-script)

(defvar indium-script--test-fs
  '(".indium"
    ("js" ("foo.js" "foo.js.map" "bar.js")))
  "Fake filesystem used in script tests.")

(describe "Looking up scripts"
  (it "should be able to retrieve parsed scripts url"
    (with-fake-indium-connection
      (indium-script-add-script-parsed "1" "foo")
      (expect (indium-script-url (indium-script-find-by-id "1")) :to-equal "foo")))

  (it "should be able to retrieve parsed scripts sourcemap url"
    (with-fake-indium-connection
      (indium-script-add-script-parsed "1" "foo" "foo-map")
      (expect (indium-script-sourcemap-url (indium-script-find-by-id "1")) :to-equal "foo-map")))

  (it "should be able to retrieve parsed scripts ids"
    (with-fake-indium-connection
      (indium-script-add-script-parsed "1" "foo")
      (expect (indium-script-id (indium-script-find-from-url "foo")) :to-equal "1")))

  (it "should be able to return all scripts with a sourcemap"
    (with-fake-indium-connection
      (indium-script-add-script-parsed "1" "foo")
      (indium-script-add-script-parsed "2" "bar" "bar.map")
      (expect (seq-map #'indium-script-id
		       (indium-script-all-scripts-with-sourcemap))
	      :to-equal '("2"))))

  (it "should be able to find the sourcemap file for a script"
    (assess-with-filesystem indium-script--test-fs
      (with-fake-indium-connection
	(indium-script-add-script-parsed "1" "http://localhost/js/foo.js" "foo.js.map")
	(spy-on 'indium-repl-get-buffer :and-return-value (find-file-noselect (expand-file-name ".")))
	(let ((script (indium-script-find-by-id "1")))
	  (expect (indium-script--sourcemap-file script)
		  :to-equal (expand-file-name "js/foo.js.map")))))))

(describe "Adding scripts"
  (it "should not multiple scripts with the same url"
    (with-fake-indium-connection
      (indium-script-add-script-parsed "1" 'url)
      (indium-script-add-script-parsed "2" 'url)
      (expect (indium-script-id (indium-script-find-from-url 'url))
	      :to-equal "2"))))

(provide 'indium-script-test)
;;; indium-script-test.el ends here
