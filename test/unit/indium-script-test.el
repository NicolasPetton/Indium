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
(require 'indium-script)

(describe "Looking up scripts"
  (it "should be able to retrieve parsed scripts url"
    (with-fake-indium-connection
     (indium-script-add-script-parsed "1" "foo")
     (expect (map-elt (indium-script-get "1") 'url) :to-equal "foo")))

  (it "should be able to retrieve parsed scripts sourcemap url"
    (with-fake-indium-connection
     (indium-script-add-script-parsed "1" "foo" "foo-map")
     (expect (map-elt (indium-script-get "1") 'sourcemap-url) :to-equal "foo-map")))

  (it "should be able to retrieve parsed scripts ids"
    (with-fake-indium-connection
     (indium-script-add-script-parsed "1" "foo")
     (expect (indium-script-get-id "foo") :to-equal "1"))))

(provide 'indium-script-test)
;;; indium-script-test.el ends here
