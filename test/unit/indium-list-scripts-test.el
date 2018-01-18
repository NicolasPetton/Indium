;;; indium-list-scripts-test.el --- Tests for indium-list-scripts.el  -*- lexical-binding: t; -*-

;; Copyright (C) 2017-2018  Nicolas Petton

;; Author: Nicolas Petton <nicolas@foretagsplatsen.se>

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
(require 'indium-list-scripts)

(describe "Listing entries"
  (it "Should signal an error when there is no connection"
    (let ((indium-current-connection nil))
      (expect (indium-list-scripts) :to-throw 'user-error))))

(describe "Making tabulated list entries"
  (it "Should be able to make entries"
    (spy-on 'indium-script-get-file :and-return-value nil)
    (let* ((script (make-indium-script :id "1" :url "foo.html"))
	   (entry (indium-list-scripts--make-entry script)))
      (expect entry :to-equal '("1" ["foo.html"]))))

  (it "Should make an entry with a button when there is a local file"
    (spy-on 'indium-script-get-file :and-return-value "bar.html")
    (let* ((script (make-indium-script :id "1" :url "foo.html"))
	   (entry (indium-list-scripts--make-entry script)))
      (expect (cadr (elt (cadr entry) 0)) :to-equal 'action))))

(provide 'indium-list-scripts-test)
;;; indium-list-scripts-test.el ends here
